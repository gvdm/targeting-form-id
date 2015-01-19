# Targeting an AJAX enabled form

Specifically I need a way to add an event handler to a specific form from a specific comet. There several forms on the page each to their own comet.

I would in this case like to `SetValueAndFocus(uniqueId + "-message-input", "")` /before/ submit the AJAX form rather than returning the JsCmd on the return of the AJAX call (as the code currently works).



