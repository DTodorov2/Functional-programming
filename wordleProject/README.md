# Wordle Game 🎮

This project implements the classic game **Wordle** with two modes:

## 1. **Game Mode**  
In this mode, the program generates a secret word from a predefined dictionary and the player attempts to guess it. The program provides feedback on each guess using colors:
- **Gray**: The letter is not in the word.
- **Yellow**: The letter is in the word but in the wrong position.
- **Green**: The letter is in the correct position.

Additional difficulty levels are supported:
- **Easy**: The program helps by warning the user about invalid words or conflicting guesses.
- **Expert**: The program can "lie" once per game with an incorrect color response.

## 2. **Assistant Mode**  
In this mode, the program tries to guess a secret word chosen by the player. For each turn, the program selects a word that maximizes the elimination of possible words. The program ensures that all previous feedback is considered and attempts to narrow down the possibilities by selecting words that eliminate the most options.

Bonus: **Expert Mode** allows one incorrect response in the feedback.

### Features:
- Wordlist-based secret word generation.
- Dynamic difficulty levels with helpful warnings and deceptive answers.
- Smart guessing algorithm in Assistant Mode that optimizes word elimination.
