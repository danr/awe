`Agda.TypeChecking.Monad.Base`:

Defines `HighlightingLevel`, use `NonInteractive`.
Defines `HighlightingMethod`, use `Direct`.
Defines `setInteractionOutputCallback`.

`Agda.Interaction.InteractionTop`:

Defines `IOTCM`, which is the command: the `FilePath`, the highlighting level
and method, and the `Interaction`, which is also defined in this file.  The
frontend should talk a layer above `Interaction`.

`Agda.Interaction.Response`:

Defines `type InteractionOutputCallback = Response -> TCM ()`
Defines `Response`. This could more or less be sent directly to the frontend.
`
