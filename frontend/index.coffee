$ ->
    pairs = (m) -> ([k,v] for k,v in m)

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
        resp = data.Response
        console.log resp
        if rngs = resp.Resp_HighlightingInfo?[0].ranges
            for [[l,u],o] in rngs
                add_cls = (cl) ->
                    console.log abstoline(l),abstoline(u),cl,o
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


    typecheck = (cm) ->
        console.log "load", cm
        abstoline = abs_to_line cm.doc.getValue()
        ws.send JSON.stringify
            Typecheck: cm.doc.getValue()

    km =
        'Ctrl-A': (cm) -> console.log "auto", cm
        'Ctrl-C': (cm) -> console.log "case", cm
        'Ctrl-,': (cm) -> console.log "goal", cm
        'Ctrl-.': (cm) -> console.log "goal and inferred", cm
        'Ctrl-Space': (cm) -> console.log "give", cm
        'Ctrl-L': typecheck

    cm = CodeMirror ((elt) -> $("#area").append elt) ,
        value: """
            module Test where

            data Nat : Set where
                zero : Nat
                suc  : Nat → Nat

            f : {A : Set} → A → A
            f x = {! !}
            """
            ###
            data Empty : Set where

            data Nat : Set where
                zero : Nat
                suc  : Nat -> Nat


            bot-elim : {A : Set} -> Empty -> A
            bot-elim ()

            _+_ : Nat -> Nat -> Nat
            zero + b = b
            suc a + b = suc (a + b)
            ###
        extraKeys: km
        cursorBlinkRate: 0

    setTimeout (-> typecheck(cm)), 300
