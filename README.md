# chess

Play chess with algebraic notation in a CLI! Here's some commands to run:

```
  debug: Runs the currently set debug statement
  q(uit): Quits the current program
  help: Prints this message
  show: Shows the current board
  [input]: Plays the chess move specified in algebraic notation
```

### Example output

```
(White to play) show
♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
(White to play) d4
Played: d4
(Black to play) d5
Played: d5
(White to play) show
♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
♟ ♟ ♟ _ ♟ ♟ ♟ ♟
_ _ _ _ _ _ _ _
_ _ _ ♟ _ _ _ _
_ _ _ ♙ _ _ _ _
_ _ _ _ _ _ _ _
♙ ♙ ♙ _ ♙ ♙ ♙ ♙
♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
```

### TODO LIST

- [x] Create engine
- [x] Show board
- [x] Move pawns
- [x] Move generator
- [x] Impl knights
- [x] Impl bishops
- [x] Impl rooks
- [x] Impl queens
- [x] Impl kings
- [x] Impl captures
- [x] Game state checks - check
- [ ] Game state checks - checkmate
- [ ] Game state checks - draw
- [ ] Disambiguate moves
- [x] Disambiguate captures
- [ ] Add quickcheck tests for board access
- [ ] Add quickcheck tests for parser
- [ ] Add quickcheck tests for rules (moves)
- [ ] Add quickcheck tests for rules (captures)
- [ ] Special move - en passant
- [ ] Special move - castling
- [ ] Special move - promotion
- [ ] Edge case rule - can't castle while in check
- [ ] (CLI) Implement stack tracer for debugging
- [ ] (CLI) Add history nav buttons for ultimate experience
- [ ] (CLI) Add reverse search for even bigger payoff
- [x] Improve README (duhh)
