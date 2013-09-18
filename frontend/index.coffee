$ ->
    km =
        'Ctrl-,': (cm) -> console.log "goal", cm
        'Ctrl-.': (cm) -> console.log "goal and inferred", cm
        "Ctrl-'": (cm) -> console.log "typecheck", cm
        'Ctrl-Space': (cm) ->
            console.log "give", cm
            console.log line, ch, cm.doc.getValue()
            {line,ch} = cm.doc.getCursor()
            console.log cm.doc.getCursor()
            cm.doc.markText {line:line, ch:0}, {line:line,ch:ch},
                className: "test"

    cm = CodeMirror ((elt) -> $("#area").append elt) ,
        value: "module Test where\n\nf : {A : Set} -> A -> A\nf x = x"
        extraKeys: km
        cursorBlinkRate: 0
