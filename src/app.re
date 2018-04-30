[%bs.raw {|require('./app.css')|}];

module CharSet = Set.Make(Char);

type state = {
  word: string,
  guesses: CharSet.t,
};

type action =
  | Guess(char);

type gameProgress =
  | Win
  | Lost
  | Guessing;

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {word: "REASONML", guesses: CharSet.empty},
  reducer: (action, state) =>
    switch (action) {
    | Guess(letter) =>
      ReasonReact.Update({
        ...state,
        guesses: CharSet.add(letter, state.guesses),
      })
    },
  render: ({state, send}) => {
    let charsInWord =
      state.word
      |> Js.String.split("")
      |> Array.map(c => c.[0])
      |> Array.to_list
      |> CharSet.of_list;
    let wrongGuessesNo =
      CharSet.cardinal(CharSet.diff(state.guesses, charsInWord));
    let remaining = 6 - wrongGuessesNo;
    let charOrUnderscore = (c: char) =>
      if (CharSet.exists(e => e == c, state.guesses)) {
        c;
      } else {
        '_';
      };
    let hiddenWord = String.map(charOrUnderscore, state.word);
    let progress: gameProgress =
      if (remaining == 0) {
        Lost;
      } else if (hiddenWord == state.word) {
        Win;
      } else {
        Guessing;
      };
    <div className="App">
      <h2> (ReasonReact.stringToElement("Hangman")) </h2>
      <h1> (ReasonReact.stringToElement(hiddenWord)) </h1>
      (
        switch (progress) {
        | Lost => ReasonReact.stringToElement("You lost!")
        | Win => ReasonReact.stringToElement("You WON!")
        | Guessing =>
          <div>
            <input
              value=""
              onKeyDown=(
                event => {
                  let c = String.uppercase(ReactEventRe.Keyboard.key(event));
                  if (String.length(c) == 1) {
                    send(Guess(c.[0]));
                  } else {
                    ();
                  };
                }
              )
            />
            <h3>
              (
                ReasonReact.stringToElement(
                  "Guesses remaining: " ++ string_of_int(remaining),
                )
              )
            </h3>
          </div>
        }
      )
    </div>;
  },
};