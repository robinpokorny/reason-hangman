[%bs.raw {|require('./app.css')|}];

module CharSet = Set.Make(Char);

type state = {
  word: string,
  guesses: CharSet.t,
};

type action =
  | Guess(char);

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
    let charOrUnderscore = (c: char) =>
      if (CharSet.exists(e => e == c, state.guesses)) {
        c;
      } else {
        '_';
      };
    let hiddenWord = String.map(charOrUnderscore, state.word);
    <div className="App">
      <h2> (ReasonReact.stringToElement("Hangman")) </h2>
      <h1> (ReasonReact.stringToElement(hiddenWord)) </h1>
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
    </div>;
  },
};
