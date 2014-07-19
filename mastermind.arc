(l "core")

(def mastermind ()
  (withs colors  '(r o y g b v)
         secret  (n-of 4 rand-elt.colors)
         guesses 10

    (readline)
    (prn "Welcome to Mastermind. You have " guesses " guesses to diffuse the bomb.")
    (prn "The available colors are " colors)
    (prn "Example guess: \"r o y r\"")

    (catch:until zero.guesses
      (prn guesses " guesses left.")

      (withs guess  (map sym (tokens:readline))
             hits   (same-count secret guess)
             misses (near-count secret guess)

        (prn hits " correct and " misses " close misses.")
        (if (is hits 4) (throw)))

      --.guesses)

    (prn:if zero.guesses
      "I'm sorry. You died in the blast."
      "Congratulations, that's correct!")))
