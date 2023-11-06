from prettytable import PrettyTable

table = PrettyTable(["rule", "FIRST()", "FOLLOW() intermediate", "FOLLOW() summary", "SELECT()"], align='l')
table.add_rows(
    [
        ["E -> TG", "{(, i}", "(T)={+, -, #}, (E)={#}, (G)={#}+(E)", "(E)={), #}", "{(, i}"],
        ["G -> +TG | -TG | ε", "{+}; {-}; {ε}", "(T)={+, -}+(G)", "(G)={), #}", "{+}; {-}; {), #}"],
        ["T -> FS", "{(, i}", "(F)={*, /}+(T), (S)=(T)", "(T)={+, -, ), #}", "{(, i}"],
        ["S -> *FS | /FS | ε", "{*}; {/}; {ε}", "(F)={*, /}+(S)", "(S)={+, -, ), #}", "{*}; {/}; {+, -, ), #}"],
        ["F -> (E) | i", "{(}; {i}", "(E)={)}", "(F)={+, -, *, /, ), #}", "{(}; {i}"],
    ]
)

table2 = PrettyTable()
table2.add_column("", ["E", "G", "T", "S", "F"])
table2.add_column("i", ["E -> TG", "G -> -TG", "T -> FS", "", "F -> i"])
table2.add_column("+", ["", "G -> +TG", "", "S -> ε", ""])
table2.add_column("-", ["", "G -> -TG", "", "S -> ε", ""])
table2.add_column("*", ["", "", "", "S -> *FS", ""])
table2.add_column("/", ["", "", "", "S -> /FS", ""])
table2.add_column("(", ["E -> TG", "", "T -> FS", "", "F -> (E)"])
table2.add_column(")", ["", "G -> ε", "", "S -> ε", ""])
table2.add_column("#", ["", "G -> ε", "", "S -> ε", ""])

print(table)
print(table2)
