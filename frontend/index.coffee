$ ->
    pairs = (m) -> ([k,v] for k,v in m)

    zip = (as,bs) ->
        bp = 0
        res = []
        for a in as
            if bp > bp.length
                break
            else
                res.push([a,bs[bp]])
                bp++
        res

    tail = (as) -> as[1...as.length]

    coord = (y,x) ->
        line: y
        ch: x

    console.log zip "abc",tail "1234"
    console.log tail "abcde"

    abs_to_line = (s) ->
        rows = s.split('\n')
        (v) ->
            l = 0
            vc = v - 1
            res = null
            for r in rows
                if vc <= r.length
                    res =
                        line: l
                        ch: vc
                    break
                else
                    l++
                    vc-=r.length+1
            res

    abstoline = ->

    ws = new WebSocket "ws://127.0.0.1:8000/"

    ws.onmessage = (evt) ->
        resp = JSON.parse evt.data
        console.log resp
        switch resp.tag
            when "HighlightingInfo"
                if cm.doc.isClean (abstoline.generation)
                    if rngs = resp.contents[0]?.ranges
                        for [[l,u],o] in rngs
                            add_cls = (cl) ->
                                cm.doc.markText abstoline(l), abstoline(u),
                                    className: cl
                            if o.aspect
                                add_cls o.aspect.tag
                                for i in o.aspect.contents
                                    add_cls i.tag
                                    if i then add_cls "#{i.contents}#{i.tag}"
                            for other in o.otherAspects
                                add_cls other
            when "InteractionPoints"
                ips = resp.contents
                for m in cm.doc.getAllMarks()
                    if m.title || m.readOnly
                        m.clear()

                rows = cm.doc.getValue().split('\n')
                l = 0
                h = 0

                make_ip = (p,q) ->
                    {line:l1,ch:c1} = p
                    {line:l2,ch:c2} = q
                    cm.doc.markText (coord l1,c1),(coord l1,c1+2),
                        atomic: true
                        readOnly: true

                    cm.doc.markText (coord l2,c2-2),(coord l2,c2),
                        atomic: true
                        readOnly: true

                    console.log "Interaction point #{ips[h]} at",p,q
                    cm.doc.markText p,q,
                        className: "Hole"
                        title: "#{ips[h]}"
                    h++

                start = null

                for row in rows
                    c = 1
                    for [now,last] in zip (tail row), row
                        if now == "?"
                            cm.doc.replaceRange "{! !}", (coord l,c), (coord l,c+1)
                            make_ip (coord l,c), (coord l,c+5)
                            c+=4

                        if last == "{" && now == "!"
                            start = coord l,c-1

                        if last == "!" && now == "}" && start
                            make_ip start, coord l,c+1
                            start = null

                        c++
                    l++

            when "RunningInfo"
                v = resp.contents[1]
                $("#info").html v

            when "DisplayInfo"
                v = resp.contents.contents
                $("#info").html v

            when "GiveAction"
                [ ip, { contents:txt } ] = resp.contents
                for m in cm.doc.getAllMarks()
                    if m.title == "#{ip}"
                        console.log ip, txt, m
                        {from,to} = m.find()
                        console.log from, to, txt
                        for pos in [from,to]
                            for s in cm.doc.findMarksAt pos
                                s.clear() # delete the {! and !} marks
                        m.clear()
                        cm.doc.replaceRange txt, from, to
                        break

            when "MakeCaseAction"
                for m in cm.doc.getAllMarks()
                    m.clear()
                {line} = cm.doc.getCursor()
                cm.doc.setLine line, resp.contents.join("\n")
                setTimeout (-> typecheck(cm)), 100

            when "MakeCase"
                for m in cm.doc.getAllMarks()
                    m.clear()
                {line} = cm.doc.getCursor()
                cm.doc.setLine line, resp.contents[1].join("\n")
                setTimeout (-> typecheck(cm)), 100

    typecheck = (cm) ->
        console.log "load", cm
        ws.send JSON.stringify
            tag: "Typecheck"
            txt: cm.doc.getValue()
        for m in cm.doc.getAllMarks()
            m.clear()
        abstoline = abs_to_line cm.doc.getValue()
        abstoline.generation = cm.doc.changeGeneration()
        true

    interaction = (k) ->
        for m in cm.doc.findMarksAt(cm.doc.getCursor())
            if m.title
                k m
                break

    interactionText = (k) ->
        interaction (m) ->
            {from,to} = m.find()
            txt = cm.doc.getRange(from,to)
            txt = txt[2...txt.length-2]
            k m, txt

    km =
        'Ctrl-A': (cm) ->
            interactionText (m, txt) ->
                ws.send JSON.stringify
                    tag: "Auto"
                    ip:  Number m.title
                    txt: txt

        'Ctrl-C': (cm) ->
            interactionText (m, txt) ->
                if txt.trim().length > 0 # not only spaces
                    ws.send JSON.stringify
                        tag: "Case"
                        ip:  Number m.title
                        txt: txt

        'Ctrl-,': (cm) ->
            interaction (m) ->
                ws.send JSON.stringify
                    tag: "Goal"
                    ip:  Number m.title

        'Ctrl-Space': (cm) ->
            interactionText (m, txt) ->
                ws.send JSON.stringify
                    tag: "Give"
                    ip:  Number m.title
                    txt: txt

        'Ctrl-.': (cm) ->
            interactionText (m, txt) ->
                ws.send JSON.stringify
                    tag: "GoalAndInferred"
                    ip:  Number m.title
                    txt: txt

        'Ctrl-L': typecheck

    cm = CodeMirror ((elt) -> $("#area").prepend elt) ,
        value: """
            module Test where

            data _+_ (A B : Set) : Set where
                inl : (l : A) -> A + B
                inr : (r : B) -> A + B

            data _*_ (A B : Set) : Set where
                _,_ : (a : A) (b : B) -> A * B

            data Bot : Set where

            not_ : Set -> Set
            not A = A -> Bot

            deMorgan : {A B : Set} -> not A * not B -> not (A + B)
            deMorgan a b = {!!}
            """
        extraKeys: km
        cursorBlinkRate: 0

    setTimeout (-> typecheck(cm)), 300
