(ns robertluo.fly-learner.explore.examples
  "Explorations written by hand, to try the vocabulary of
  robertluo.fly-learner.explore on what the sources teach (game theory, the
  λ-calculus) before a model writes any. Plain data, as a model would send
  it (after `explore/normalize`).")

(def prisoners-dilemma
  {:title  "Prisoner's dilemma: where is the equilibrium?"
   :intro  "Each player cooperates or defects. A cell holds (row's payoff, column's payoff); highlighted cells are Nash equilibria: neither player gains by changing alone."
   :vars   [{:name "t" :init "5"} {:name "r" :init "3"} {:name "p" :init "1"} {:name "s" :init "0"}
            {:name "pick" :init "nil"}]
   :derive [{:name "A" :expr "[[r s] [t p]]"}
            {:name "B" :expr "[[r t] [s p]]"}
            {:name "moves" :expr "[\"Cooperate\" \"Defect\"]"}]
   :view   [{:type :slider :var "t" :min 0 :max 8 :label "Temptation T (defect while the other cooperates): {t}"}
            {:type :slider :var "s" :min -2 :max 4 :label "Sucker's payoff S (cooperate while the other defects): {s}"}
            {:type :table :rows ["Cooperate" "Defect"] :cols ["Cooperate" "Defect"]
             :cell "[(get-in A [row col]) (get-in B [row col])]"
             :highlight "(and (= (get-in A [row col]) (apply max (column A col)))
                              (= (get-in B [row col]) (apply max (nth B row))))"
             :on-click [{:var "pick" :expr "(if (= pick [row col]) nil [row col])"}]
             :caption "Row player chooses the row, column player the column. Click a cell."}
            {:type :text :show "(some? pick)"
             :text "At ({(nth moves (first pick))}, {(nth moves (second pick))}): row could switch to {(nth moves (- 1 (first pick)))} for {(get-in A [(- 1 (first pick)) (second pick)])} instead of {(get-in A pick)}; column to {(nth moves (- 1 (second pick)))} for {(get-in B [(first pick) (- 1 (second pick))])} instead of {(get-in B pick)}."}
            {:type :text
             :text "{(cond (and (> t r) (> p s)) \"T > R and P > S: defecting is better whatever the other does, so both defect, though both would rather cooperate. That is the dilemma.\" (<= t r) \"T ≤ R: cooperating is a best reply to cooperation, so mutual cooperation is an equilibrium too; the dilemma is gone.\" :else \"S ≥ P: cooperating against a defector is no worse, so defection is no longer dominant.\")}"}
            {:type :button :label "Back to T = 5, S = 0" :set [{:var "t" :expr "5"} {:var "s" :expr "0"}]}]})

(def mixed-strategy
  {:title  "Mixing: when is the row player indifferent?"
   :intro  "Matching pennies, with a bonus a for matching on heads. Row wins a for (H, H), 1 for (T, T) and loses 1 otherwise. Column plays heads with probability q."
   :vars   [{:name "a" :init "1"} {:name "q" :init "0.3"}]
   :derive [{:name "q*" :expr "(/ 2 (+ a 3))"}]
   :view   [{:type :predict :var "guess" :prompt "In the plain game (a = 1), which q makes heads and tails equally good for row?"
             :options ["q = 0" "q = 1/4" "q = 1/2" "q = 1"] :answer "2"
             :explain "With a = 1, heads gives 2q − 1 and tails 1 − 2q: equal at q = 1/2. Any other q makes one pure reply strictly better, so column must mix exactly so to leave row nothing to exploit."}
            {:type :slider :var "a" :min 1 :max 5 :step 0.5 :label "Bonus a for matching heads: {a}"}
            {:type :slider :var "q" :min 0 :max 1 :step 0.01 :label "Column plays heads with q = {(fmt q 2)}"}
            {:type :plot :x-min 0 :x-max 1 :y-min -1.5 :y-max 5 :x-label "q" :y-label "row's expected payoff"
             :series [{:y "(- (* (+ a 1) x) 1)" :label "Heads"} {:y "(- 1 (* 2 x))" :label "Tails"}]
             :points [{:x "q" :y "(- (* (+ a 1) q) 1)" :label "H {(fmt (- (* (+ a 1) q) 1) 2)}"}
                      {:x "q" :y "(- 1 (* 2 q))" :label "T {(fmt (- 1 (* 2 q)) 2)}"}
                      {:x "q*" :y "(- 1 (* 2 q*))" :label "indifferent"}]}
            {:type :text
             :text "The lines cross at q* = 2 / (a + 3) = {(fmt q* 3)}. Here row's best reply is {(cond (> (- (* (+ a 1) q) 1) (- 1 (* 2 q))) \"heads\" (< (- (* (+ a 1) q) 1) (- 1 (* 2 q))) \"tails\" :else \"either: row is indifferent\")}. A larger bonus makes column play heads less often in equilibrium."}]})

(def backward-induction
  {:title  "Backward induction: is the threat credible?"
   :intro  "An entrant decides whether to enter a market; if it does, the incumbent fights or accommodates. Payoffs are (entrant, incumbent)."
   :vars   [{:name "focus" :init "nil"} {:name "chosen" :init "[]"}]
   :view   [{:type :diagram :direction "down"
             :nodes [{:id "E" :label "Entrant" :highlight "(or (= focus id) (contains? (set chosen) id))"}
                     {:id "out" :label "Out\n(0, 4)" :highlight "(contains? (set chosen) id)"}
                     {:id "I" :label "Incumbent" :highlight "(or (= focus id) (contains? (set chosen) id))"}
                     {:id "fight" :label "Fight\n(−1, −1)" :highlight "(contains? (set chosen) id)"}
                     {:id "acc" :label "Accommodate\n(2, 2)" :highlight "(contains? (set chosen) id)"}]
             :edges [{:from "E" :to "out" :label "out" :highlight "(contains? (set chosen) to)"}
                     {:from "E" :to "I" :label "in" :highlight "(contains? (set chosen) to)"}
                     {:from "I" :to "fight" :highlight "(contains? (set chosen) to)"}
                     {:from "I" :to "acc" :highlight "(contains? (set chosen) to)"}]}
            {:type :steps :var "step"
             :frames [{:caption "The incumbent threatens to fight any entry. Should the entrant believe it? Solve from the end of the game."}
                      {:caption "Start at the last decision: the incumbent, once the entrant is in."
                       :set [{:var "focus" :expr "\"I\""}]}
                      {:caption "Fighting gives the incumbent −1, accommodating 2: it accommodates."
                       :set [{:var "chosen" :expr "[\"acc\"]"}]}
                      {:caption "Now the entrant compares out (0) with in, which leads to (2, 2)."
                       :set [{:var "focus" :expr "\"E\""}]}
                      {:caption "It enters. The subgame-perfect equilibrium is (in, accommodate): the threat to fight is not credible, since carrying it out would hurt the incumbent."
                       :set [{:var "chosen" :expr "[\"I\" \"acc\"]"} {:var "focus" :expr "nil"}]}]}]})

(def beta-reduction
  {:title  "Reduction order: normal or applicative?"
   :intro  "K = λx. λy. x throws its second argument away; Ω = (λw. w w) (λw. w w) reduces only to itself. Reduce K z Ω step by step, the redex marked."
   :vars   [{:name "strategy" :init "\"normal\""} {:name "marks" :init "true"}
            {:name "term" :init "\"(λx. λy. x) z ((λw. w w) (λw. w w))\""}
            {:name "redex" :init "\"(λx. λy. x) z\""}]
   :derive [{:name "normal?" :expr "(= strategy \"normal\")"}]
   :view   [{:type :choice :var "strategy" :label "Strategy"
             :options [{:label "Normal order (leftmost outermost)" :value "\"normal\""}
                       {:label "Applicative order (arguments first)" :value "\"applicative\""}]}
            {:type :toggle :var "marks" :label "Mark the redex"}
            {:type :formula :text "{term}" :mark "{(if marks redex \"\")}"}
            {:type :steps :var "step"
             :frames [{:caption "The leftmost outermost redex is K applied to z."}
                      {:caption "{(if normal? \"K z becomes λy. z. Normal order applies it to Ω at once, without reducing Ω.\" \"K z becomes λy. z. Applicative order first reduces the argument Ω.\")}"
                       :set [{:var "term" :expr "\"(λy. z) ((λw. w w) (λw. w w))\""}
                             {:var "redex" :expr "(if normal? term \"(λw. w w) (λw. w w)\")"}]}
                      {:caption "{(if normal? \"y does not occur in z, so Ω is thrown away: the normal form is z.\" \"Ω reduces to itself, so the same term comes back, forever: applicative order never terminates here.\")}"
                       :set [{:var "term" :expr "(if normal? \"z\" term)"}
                             {:var "redex" :expr "(if normal? \"\" redex)"}]}]}
            {:type :text :text "{(if normal? \"Normal order finds a normal form whenever one exists (standardisation).\" \"A term can have a normal form that applicative order never reaches.\")}"}]})

(def all
  "name -> spec."
  {"prisoners-dilemma"  prisoners-dilemma
   "mixed-strategy"     mixed-strategy
   "backward-induction" backward-induction
   "beta-reduction"     beta-reduction})
