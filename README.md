# ELIZA: THE SESSION

```
  E L I Z A  :  T H E  S E S S I O N
  ──────────────────────────────────
  A psychological interactive fiction.
  You are a patient. The therapist is waiting.
```

> *"How do you do. Please state your problem."*

---

## What is this?

**ELIZA: The Session** is a single-file interactive fiction game written in Common Lisp (SBCL).
You sit across from a retro text-based therapist — and over the course of a 15–25 minute session,
the conversation slowly unravels a repressed memory you didn't know you were carrying.

The game runs entirely in your terminal. No graphics. No sound. Just green text on black,
a typewriter effect, and the creeping feeling that ELIZA knows more than she should.

---

## Requirements

- **SBCL** (Steel Bank Common Lisp) — [https://www.sbcl.org](https://www.sbcl.org)

### Installing SBCL

| Platform       | Command                                      |
|----------------|----------------------------------------------|
| Arch / Manjaro | `sudo pacman -S sbcl`                        |
| Debian / Ubuntu| `sudo apt install sbcl`                      |
| Fedora / RHEL  | `sudo dnf install sbcl`                      |
| macOS (Homebrew)| `brew install sbcl`                         |
| Windows        | Download installer from [sbcl.org/platform-table.html](http://www.sbcl.org/platform-table.html) |

---

## Running the Game

```bash
sbcl --script eliza.lisp
```

That's it. One file. No dependencies. No build step.

---

## Controls

| Input         | Action                         |
|---------------|--------------------------------|
| *(just type)* | Speak to the therapist         |
| `quit`        | End the session              |
| `help`        | Show command reference         |
| `log`         | View live session summary      |
| `Enter` (blank)| Let silence speak             |

---

## How It Works

### The Narrative

You are a patient visiting a therapist. The session opens innocuously — standard intake questions,
therapeutic deflection. But ELIZA's responses grow more specific. She begins referencing things
you haven't told her: a lake, a summer, a name.

**Sam.**

As the session deepens, a repressed memory surfaces: a childhood accident you witnessed —
and survived — and have never spoken of. The story you uncover depends entirely on what you say.

### The Engine

- **Pattern matching** — 50+ keyword rules scan your input for emotionally significant words
  (`lake`, `dream`, `guilty`, `mother`, `Sam`, etc.)
- **Three main stages** `:intake` → `:exploration` → `:revelation`, plus a `:crisis` stage
  if the session deteriorates
- **Word mirroring** — ELIZA reflects your own language back at you
  (*"You said 'drown' — what does that mean to you?"*)
- **Resistance tracking** — evasive answers are noticed and challenged
- **Confession detection** — 15 direct-confession phrases trigger state changes
- **Ambient events** — atmospheric interrupts fire between turns (flickering lights, static,
  clock stopping)
- **Score system** — a hidden `dread/hope` counter shifts with every keyword;
  it determines which ending you reach

### The Endings

There are **five** distinct endings:

| Ending          | How to reach it                                          |
|-----------------|----------------------------------------------------------|
| **BREAKTHROUGH** | Confess, keep hope; score stays above zero              |
| **CATHARSIS**    | Confess, but arrive there with heavy dread              |
| **DISSOCIATION** | Score collapses into crisis; you stop responding        |
| **BREAKDOWN**    | Maximum dread; the session corrupts                     |
| **LOOP**         | 28+ turns without resolution; the clock resets          |

A **transcript** of every session is automatically saved to `transcript.txt`
in the directory you run the game from.

---

## Tips

- You don't need to know the "right" answers — speak naturally
- Short, evasive responses are noticed
- The word `Sam` matters
- Some words carry more weight than others: `lake`, `water`, `summer`, `guilty`, `dream`
- The ending is earned, not triggered — the score accumulates across the whole session
- Looping sessions carry dread forward; the second session is different from the first

---

## Technical Notes

- **Single file** — all game data (rules, responses, state machine, rendering) lives in `eliza.lisp`
- **ANSI terminal** — requires a terminal emulator that supports ANSI escape codes
  (any modern Linux/macOS terminal; Windows Terminal on Windows 10+)
- **No external dependencies** — uses only SBCL's built-in `uiop` package
- Tested on SBCL 2.3.x / Linux

---

## File Structure

```
eliza.lisp       ← The entire game
README.md        ← This file
transcript.txt   ← Auto-generated after each session
```

---

## Credits

Inspired by Joseph Weizenbaum's original **ELIZA** (1966, MIT),
the first chatbot — and one of the first programs to demonstrate that
humans project meaning and empathy onto machines that have none.

This game asks: what if it did have some?

---

## License

**ELIZA: The Session** is released as freeware for personal use.
Do whatever you like with it. If you find it meaningful, that's enough.

---

```
  [ Some sessions end. Some do not. ]
```
