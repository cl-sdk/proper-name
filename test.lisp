(defpackage #:proper-name.test
  (:use #:cl))

(in-package :proper-name.test)

(5am:def-suite proper-name.suite
  :description "Testing proper names.")

(5am:in-suite proper-name.suite)

(5am:def-test full-names ()
  (dolist (name '("Jean-Luc Picard"
                  "François Dupont"
                  "Łukasz Kowalski"
                  "Μαρία Παπαδοπούλου"
                  "Иван Петрович Иванов"
                  "محمد علي"
                  "עִבְרִית כהן"
                  "O’Connor Smith"
                  "José da Silva"
                  "Miyamoto Musashi"
                  "Nguyễn Văn A"
                  "Chloë Moretz"
                  "Björn Ironside"
                  "Søren Kierkegaard"
                  "René Descartes"
                  "Alfredo Rodríguez"
                  "Tomáš Garrigue Masaryk"
                  "André Gide"
                  "Camilo José Cela"
                  "Hans Zimmer"
                  "Jan van Eyck"
                  "Abdul Rahman Al-Sudais"
                  "Niccolò Machiavelli"
                  "Elżbieta Kowalczyk"
                  "Grigori Rasputin"
                  "Mikhail Gorbachev"
                  "Alexander Pushkin"
                  "Leon Trotsky"
                  "Mahmoud Darwish"
                  "Fatima Zahra"
                  "Ali ibn Abi Talib"
                  "Immanuel Kant"
                  "Johann Wolfgang von Goethe"
                  "Friedrich Nietzsche"
                  "Heinrich Heine"
                  "Albert Einstein"
                  "Marie Curie"
                  "Frédéric Chopin"
                  "Claude Debussy"
                  "Jean-Jacques Rousseau"
                  "Jean-Paul Sartre"
                  "Simone de Beauvoir"
                  "Charles Baudelaire"
                  "Paul Cézanne"
                  "Vincent van Gogh"
                  "Ludwig van Beethoven"
                  "Wolfgang Amadeus Mozart"
                  "Johann Sebastian Bach"
                  "Pyotr Ilyich Tchaikovsky"))
    (5am:is (proper-name:valid-name-unicode-p name))))

(5am:def-test single-names ()
  (dolist (name '("Jean"
                  "François"
                  "Łukasz"
                  "Μαρία"
                  "Иван"
                  "محمد"
                  "עִבְרִית"
                  "O’Connor"
                  "Björn"
                  "Chloë"))
    (5am:is (proper-name:valid-name-unicode-p name))))

(5am:def-test invalid-names ()
  (dolist (name '("Jean-Luc Picard!"
                  "François123 Dupont"
                  "Łukasz@Kowalski"
                  "Μαρία#Παπαδοπούλου"
                  "Иван$Иванов"
                  "محمد%علي"
                  "עִבְרִית^כהן"
                  "O’Connor*Smith"
                  "Dr. Jean-Luc Picard"
                  "123 Ali"
                  "Jane.Doe"
                  "John_Doe"
                  "Mary+Sue"
                  "Robert=Smith"
                  "François 😀 Dupont"
                  "Łukasz 😉 Kowalski"
                  "Иван🚀Иванов"
                  "محمد💡علي"
                  "עִבְרִית🔥כהן"
                  "Smith@Example"))
    (5am:is (not (proper-name:valid-name-unicode-p name)))))

(5am:def-test ligature-names ()
  (dolist (name '("Ægir Jónsson"                ; AE ligature
                  "Œuvre d’art"                 ; OE ligature + apostrophe
                  "façade"                      ; with cedilla
                  "ﬀi Test"                     ; U+FB03 LIGATURE (ffi) → normalized
                  "ﬂorence Nightingale"         ; U+FB02 (fl ligature)
                  "Ｆｕｌｌｗｉｄｔｈ Ｎａｍｅ"    ; fullwidth letters
                  "Straße Müller"               ; ß German sharp-S
                  "Crème Brûlée"                ; accents
                  "Dvořák Antonín"              ; Czech name with caron
                  "Gödel Escher Bach"))         ; umlaut
    (5am:is (proper-name:valid-name-unicode-p name))))

(5am:def-test zalgo-names ()
  (dolist (name '("Ḧ̶͉͍́̽̽͝e̴̎͛́͋͝l̷̄̊͊̍͝o̶͉͒"
                  "A͂͂͂͂͂͂͂nna"
                  "M̄̄̄̄aria"
                  "Z͑͐͛͒͞͞͞͞͞a͂͂͂͂͂͂l͗͗͗g͊͊͊o"
                  "E͠͠͠mil"
                  "N͛͛͛͛͛ame"
                  "J͘͘͘͘ohn"
                  "B̅̅̅̅̅̅̅ob"
                  "C͟͟͟͟͟arol"
                  "Da̸̸̸̸vid"
                  "M͢͢͢͢a͜͜͜rk"
                  "S͟͟͟͟usan"
                  "T͡͡͡͡om"
                  "R͏͏͏͏͏alph"
                  "O͟͟͟scar"))
    (5am:is (not (proper-name:valid-name-unicode-p name)))))
