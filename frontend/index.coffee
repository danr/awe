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
        data = JSON.parse evt.data
        console.log data
        resp = data.Response
        console.log resp
        if rngs = resp.Resp_HighlightingInfo?[0].ranges
            for [[l,u],o] in rngs
                add_cls = (cl) ->
                    # console.log abstoline(l),abstoline(u),cl,o
                    cm.doc.markText abstoline(l), abstoline(u),
                        className: cl
                for k,v of o.aspect
                    add_cls k
                    if k == "Name"
                        for nm,sp of v[0]
                            add_cls nm
                            for ty,_ of sp
                                cl = "#{ty}#{nm}"
                                add_cls cl
                for other in o.otherAspects
                    for k,v of other
                        add_cls k

        if ips = resp.Resp_InteractionPoints
            for m in cm.doc.getAllMarks()
                if m.title || m.readOnly
                    m.clear()

            console.log ips
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

                cm.doc.markText p,q,
                    className: "Hole"
                    title: "#{h}"
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
                        console.log start

                    if last == "!" && now == "}" && start
                        make_ip start, coord l,c+1
                        start = null

                    c++
                l++

        if info = resp.Resp_DisplayInfo
            for _,v of info
                console.log v
                $("#info").html v

        if ref = resp.Resp_GiveAction
            console.log "give", ref
            [ ip, o ] = ref
            console.log ip, o
            if ip? && o?
                for k,s of o
                    console.log k,s
                    for m in cm.doc.getAllMarks()
                        if m.title == "#{ip}"
                            console.log m
                            {from,to} = m.find()
                            console.log from, to, s
                            cm.doc.replaceRange s, from, to
                            m.clear()


    typecheck = (cm) ->
        console.log "load", cm
        abstoline = abs_to_line cm.doc.getValue()
        ws.send JSON.stringify
            Typecheck: cm.doc.getValue()
        for m in cm.doc.getAllMarks()
            m.clear()
        true

    interaction = (k) ->
        for m in cm.doc.findMarksAt(cm.doc.getCursor())
            if m.title
                k m

    km =
        'Ctrl-A': (cm) -> console.log "auto", cm
        'Ctrl-C': (cm) -> console.log "case", cm
        'Ctrl-,': (cm) ->
            interaction (m) ->
                ws.send JSON.stringify
                    Goal: Number m.title

        'Ctrl-Space': (cm) ->
            interaction (m) ->
                {from,to} = m.find()
                txt = cm.doc.getRange(from,to)
                txt = txt[2...txt.length-2]
                console.log txt, m
                o = Give:
                        [ Number m.title
                          txt
                        ]
                console.log o, JSON.stringify o
                ws.send JSON.stringify o


        'Ctrl-.': (cm) -> console.log "goal and inferred", cm
        'Ctrl-L': typecheck

    cm = CodeMirror ((elt) -> $("#area").prepend elt) ,
        value: """
            module Test where

            data Nat : Set where
                zero : Nat
                suc  : Nat → Nat

            f : {A : Set} → A → A
            f x = ?

            g : {A : Set} → A → A
            g x = {! !}

            h : {A : Set} → A → A
            h x = ?

            data Empty : Set where

            bot-elim : {A : Set} -> Empty -> A
            bot-elim ()

            _+_ : Nat -> Nat -> Nat
            zero + b = b
            suc a + b = suc (a + b)
            """
        extraKeys: km
        cursorBlinkRate: 0

    setTimeout (-> typecheck(cm)), 300
