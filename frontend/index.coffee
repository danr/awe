$ ->
    ws = new WebSocket "ws://127.0.0.1:8000/"

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

    clear_marks = ->
        for m in cm.doc.getAllMarks()
            m.clear()

    highlight_gen = null

    typecheck = ->
        console.log "load", cm
        ws.send JSON.stringify
            tag: "Typecheck"
            txt: cm.doc.getValue()
        clear_marks()
        highlight_gen = cm.doc.changeGeneration()
        true

    highlight_info = (resp) ->
        if (cm.doc.isClean highlight_gen) && rngs = resp.contents[0]?.ranges
            for [[l,u],o] in rngs
                add_cls = (cl) ->
                    cm.doc.markText cm.doc.posFromIndex(l-1), cm.doc.posFromIndex(u-1),
                        className: cl
                if o.aspect
                    add_cls o.aspect.tag
                    for i in o.aspect.contents
                        add_cls i.tag
                        if i then add_cls "#{i.contents}#{i.tag}"
                for other in o.otherAspects
                    add_cls other

    # TODO: incomplete due to comments and nested holes
    interaction_points = (ips) ->
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

    set_info = (info) -> $("#info").html info

    ws.onopen = ->
        set_info "Connected"
        typecheck()

    give_action = (ip,txt) ->
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

    make_case_action = (lines) ->
        clear_marks()
        {line} = cm.doc.getCursor()
        cm.doc.setLine line, lines.join("\n")
        setTimeout typecheck, 10 # wait a little for editor to update

    ws.onmessage = (evt) ->
        resp = JSON.parse evt.data
        console.log resp
        switch resp.tag
            when "HighlightingInfo"
                highlight_info resp
            when "InteractionPoints"
                interaction_points resp.contents
            when "RunningInfo"
                set_info resp.contents[1]
            when "DisplayInfo"
                set_info resp.contents.contents
            when "GiveAction"
                [ip,{contents:txt}] = resp.contents
                give_action ip, txt
            when "MakeCaseAction"
                make_case_action resp.contents
            when "MakeCase"
                make_case_action resp.contents[1]

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

    interactionTextCmd = (cmd,not_only_spaces,do_trim) ->
        interactionText (m, txt) ->
            if (not not_only_spaces) || txt.trim().length > 0
                ws.send JSON.stringify
                    tag: cmd
                    ip:  Number m.title
                    txt: if do_trim then txt.trim() else txt

    km =
        'Ctrl-R':     -> interactionTextCmd "Refine", false, true
        'Ctrl-A':     -> interactionTextCmd "Auto"
        'Ctrl-C':     -> interactionTextCmd "Case", true
        'Ctrl-N':     -> interactionTextCmd "Normalise", true
        'Ctrl-,':     -> interactionTextCmd "Goal"         # sends txt too but does not matter
        'Ctrl-.':     -> interactionTextCmd "GoalAndInferred", true
        'Ctrl-Space': -> interactionTextCmd "Give", true
        'Ctrl-L':     -> typecheck()
        'Ctrl-D':     -> clear_marks()

    cm = CodeMirror ((elt) -> $("#area").prepend elt) ,
        value: """
            module Test where

            {-
                Ctrl-L     : typecheck
                Ctrl-Space : give
                Ctrl-.     : goal
                Ctrl-,     : goal and inferred
                Ctrl-N     : normalise
                Ctrl-C     : case
                Ctrl-A     : auto
                Ctrl-R     : refine
                Ctrl-D     : deactivate highlighting
            -}

            data Nat : Set where
              zero : Nat
              suc : (n : Nat) → Nat

            data _==_ (x : Nat) : Nat → Set where
              refl : x == x

            data _/=_ : Nat → Nat → Set where
              z/=s : ∀ {n} → zero /= suc n
              s/=z : ∀ {n} → suc n /= zero
              s/=s : ∀ {m n} → m /= n → suc m /= suc n

            data Equal (m n : Nat) : Set where
              yes : m == n → Equal m n
              no : m /= n → Equal m n

            data ⊥ : Set where

            isEqual : (m n : Nat) → Equal m n
            isEqual = {!!}

            equality-disjoint : (m n : Nat) → m == n → m /= n → ⊥
            equality-disjoint = {!!}
            """
        extraKeys: km
        cursorBlinkRate: 0

    # setTimeout typecheck, 300

    ###
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
    ###
