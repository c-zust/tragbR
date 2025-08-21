# Tragbarkeitsrechner


Dieses Repository enthält ein formales Modell und eine Implementierung in R zur Berechnung des maximal tragbaren Kaufpreises einer Immobilie basierend auf Vermögen und Einkommen. Das Modell basiert auf dem [Tragbarkeitsrechner](https://www.zkb.ch/de/private/hypotheken-immobilien/rechner-hilfsmittel/hypothekenrechner.html/) der Zürcher Kantonal Bank und berücksichtigt Eigenmittelanforderungen, Unterhaltskosten, Zinszahlungen und Amortisation über einen definierten Zeitraum. 

Zum einen soll damit die zugrunde liegende Methodik zur Berechnung von Tragbarkeitsabklärungen transparent und nachvollziehbar gemacht werden. Zum anderen soll der Rechner Forschenden beispielsweise die Möglichkeit bieten, zu analysieren, wie sich die Leistbarkeit von Wohneigentum aus Nachfragesicht im Zeitverlauf verändert hat – mit anderen Worten: Was konnten sich Haushalte leisten und wie hat sich dies über die Jahre gewandelt? Dies ist nur ein Beispiel von vielen möglichen Anwendungsfeldern, für die dieser Tragbarkeitsrechner als Grundlage dienen kann.

> **Note**: Das Package wird nicht vom statistischen Amt des Kantons Zürich weiterentwickelt. Bei Fragen kann man sich jedoch gerne an [datashop@statistik.zh.ch](mailto:datashop@statistik.zh.ch) wenden oder ein Issue erfassen und `c-zust` taggen. 

 
## Installation

```
remotes::install_github("statistikZH/tragbR",
                        ref = "main")
```

## Inhalte
- Formale Herleitung: Mathematische Formulierung des Optimierungsproblems inklusive Fallunterscheidungen.

- R-Implementierung: Lösung des Optimierungsproblems mit `nloptr`.

- `get`-Fuktionen: Funktionen zur direkten Berechnung des optimalen Kaufpreises (y) und Fremdkapitalanteils (z).


## Hauptfunktionen
Zur praktischen Anwendung stehen folgende zentrale Funktionen zur Verfügung:

#### `get_optimal_values()`
Berechnet den optimalen Kaufpreis (y) sowie den dazugehörigen optimalen Fremdfinanzierungsanteil (z) basierend auf Einkommen, Vermögen und den Modellparametern.

➡️ Empfohlene Standardfunktion für typische Anwendungsfälle.

#### `get_optimal_y()`
Gibt ausschließlich den optimal tragbaren Kaufpreis (y) zurück

#### `get_optimal_z()`
Gibt ausschließlich den optimalen Fremdfinanzierungsanteil (z) zurück.

#### `solve_max_problem()`
Gibt die detaillierten Resultate beider Maximierungsprobleme (mit und ohne Amortisation) zurück – inklusive Iterationsanzahl, Constraints und Zwischenwerte.

➡️ Nützlich zur Diagnose und Feinjustierung der Berechnungslogik.

```
# Beispiel:
get_optimal_values(V = 100000, E = 34000, r = 0.05, u = 0.007,
                   alpha = 0.35, min_eigenmittel = 0.2,
                   amortisation_jahre = 15, amortisation_grenze = 2/3,
                   z0 = 0.5)
```

> **Note**: Siehe **[Formalisierung](https://statdm.ji.ktzh.ch:8788/STAT/tragbarkeitsrechner/src/branch/main/Formalisierung.html)** für die ausführliche Herleitung und weitere Anwendungsbeispiele :bulb:




## Voraussetzungen
- R (>= 4.0)
- Packages: nloptr


