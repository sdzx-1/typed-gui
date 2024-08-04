# typed-gui
GUI framework based on typed-fsm

Similar to the elm architecture, the difference is that typed-gui separates control status and data status.

There are at least three advantages to doing this.

1. The type of the View part has a clear control state, which can limit the type of Message and avoid sending error messages.

2. The Update part can give full play to the advantages of typed-fsm, and typed-fsm takes over the entire control flow.

3. Extract the common part and simplify the control state.
