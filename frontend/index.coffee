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
            console.log ips
            rows = cm.doc.getValue().split('\n')
            l = 0
            h = 0
            for row in rows
                c = 1
                for [now,next] in zip (tail row), row
                    if now == "?"
                        console.log l,c,now,next

                        cm.doc.replaceRange "  ", (coord l,c), (coord l,c+1)

                        cm.doc.replaceRange "!}", coord l,c+1
                        cm.doc.markText (coord l,c+1),(coord l,c+3),
                            atomic: true
                            readOnly: true
                        cm.doc.replaceRange "{! ", coord l,c
                        cm.doc.markText (coord l,c),(coord l,c+2),
                            atomic: true
                            readOnly: true

                        cm.doc.markText (coord l,c),(coord l,c+6),
                            className: "Hole"
                            title: "#{h}"
                        h++

                    c++
                l++

        if info = resp.Resp_DisplayInfo
            for _,v of info
                console.log v
                $("#info").html v


    typecheck = (cm) ->
        console.log "load", cm
        abstoline = abs_to_line cm.doc.getValue()
        ws.send JSON.stringify
            Typecheck: cm.doc.getValue()
        for m in cm.doc.getAllMarks()
            m.clear()
        true

    km =
        'Ctrl-A': (cm) -> console.log "auto", cm
        'Ctrl-C': (cm) -> console.log "case", cm
        'Ctrl-,': (cm) ->
            console.log "goal", cm
            for m in cm.doc.findMarksAt(cm.doc.getCursor())
                console.log m.title
                if m.title
                    console.log (Number m.title)
                    ws.send JSON.stringify
                        Goal: Number m.title


        'Ctrl-.': (cm) -> console.log "goal and inferred", cm
        'Ctrl-Space': (cm) -> console.log "give", cm
        'Ctrl-L': typecheck

    cm = CodeMirror ((elt) -> $("#area").prepend elt) ,
        value: """
            module Test where

            data Nat : Set where
                zero : Nat
                suc  : Nat → Nat

            f : {A : Set} → A → A
            f x = ?

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
