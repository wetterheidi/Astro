# Dokumentation der Funktion `calculateSkyBrightness`

## Überblick
Die Funktion `calculateSkyBrightness` berechnet die Helligkeit des Nachthimmels in **Millilux (mlx)** basierend auf der Mondphase, Mondhöhe, geografischen Position und Zeitpunkt der Beobachtung. Sie berücksichtigt den Beitrag des Mondlichts unter Verwendung eines dynamischen Winkelabstands (\( \rho \)) und einer physikalisch motivierten Streufunktion. Die Funktion ist optimiert für astronomische Anwendungen und liefert präzise Ergebnisse für Vollmond, Sichelmond und mittlere Mondphasen, kalibriert anhand empirischer Daten.

## Zweck
- Berechnung der Nachthimmelhelligkeit für klare (wolkenlose) und bewölkte Bedingungen in Millilux (mlx).
- Unterstützung von astronomischen Beobachtungen durch realistische Modellierung des Mondlichtbeitrags.
- Integration in Tabellenanzeigen für monatliche Helligkeitsanalysen.

## Eingaben
- **`time`**: Zeitpunkt der Beobachtung (z. B. `Date`-Objekt), verwendet zur Berechnung von Sonnen- und Mondposition.
- **`lat`**: Geografische Breite des Beobachtungsorts in Grad (positiv für nördliche Breite, negativ für südliche).
- **`lon`**: Geografische Länge des Beobachtungsorts in Grad (positiv für östliche Länge, negativ für westliche).
- **`moonIllum`**: Objekt mit dem Schlüssel `fraction`, der den beleuchteten Anteil des Mondes angibt (Wert zwischen 0 und 1).
- **`moonPos`**: Objekt mit den Schlüsseln:
  - `altitude`: Mondhöhe über dem Horizont in Radiant.
  - `azimuth`: Azimut des Mondes in Radiant (nicht verwendet in der aktuellen Version).

## Ausgaben
- **Rückgabewert**: Ein Objekt mit zwei Schlüsseln:
  - `brightnessW`: Helligkeit in Millilux (mlx) für wolkenlose Bedingungen.
  - `brightnessB`: Helligkeit in Millilux (mlx) für bewölkte Bedingungen.
- **Einheit**: Millilux (mlx).

## Algorithmus
Die Funktion kombiniert astronomische Modelle und empirische Kalibrierungen:
1. **Mondhöhe und Zenitdistanz**:
   - Mondhöhe (\( \text{moonElevation} \)) wird aus `moonPos.altitude` in Grad umgerechnet.
   - Zenitdistanz: \( \text{zenithDistance} = 90^\circ - \text{moonElevation} \).
   - Winkelabstand (\( \rho \)): \( \rho = \text{zenithDistance} \), dynamisch berechnet.
2. **Airmass**:
   - Näherung: \( \text{airmass} = \frac{1}{\cos(\text{zenithDistance})} \) für \( \text{zenithDistance} \leq 80^\circ \), sonst 40.
3. **Mondmagnitude**:
   - Nach Krisciunas & Schaefer: \( m_{\text{Moon}} = -12.73 + 0.026 \cdot |\text{phaseAngle}| + 4 \cdot 10^{-9} \cdot \text{phaseAngle}^4 \).
   - Phasenwinkel: \( \text{phaseAngle} = \arccos(2 \cdot \text{moonFraction} - 1) \cdot \frac{180}{\pi} \).
4. **Streufunktion**:
   - \( C(\rho) = 1 \cdot 10^{-6} \cdot (1 + \cos(\rho)) \cdot \text{moonFraction} \), in nanoLamberts (nL).
   - Physikalisch motiviert, modelliert die Streuung des Mondlichts in Abhängigkeit vom Winkelabstand.
5. **Flux-Berechnung**:
   - Umrechnung in cd/m²: \( \text{flux_cd_m2} = C(\rho) \cdot 10^{-9} \cdot 3.183 \cdot 10^{-3} \).
   - Helligkeit in mag/arcsec²: \( \text{moonBrightness} = -2.5 \cdot \log_{10}\left(\frac{\text{flux_cd_m2}}{3.28 \cdot 10^{-6}}\right) + m_{\text{Moon}} + k \cdot \text{airmass} \).
   - Extinktionskoeffizient: \( k = 0.3 \) mag/airmass.
6. **Summierung**:
   - Grundhelligkeit: 22.0 mag/arcsec² (wolkenlos).
   - Gesamtflux: Kombination von Grundhelligkeit und Mondlichtbeitrag.
7. **Umrechnung in Millilux**:
   - Wolkenlos: \( F_{\text{mlx}} = 10^{-0.4 \cdot (m - 22.4775)} \).
   - Bewölkt: \( F_{\text{mlx,b}} = F_{\text{mlx,w}} \cdot 0.25 \), basierend auf empirischen Daten.
8. **Kategorisierung**:
   - Werte werden in Kategorien (`B`, `D`, etc.) eingeteilt basierend auf Schwellenwerten.

## Leistung
Die Funktion wurde anhand empirischer Daten kalibriert, um folgende Zielwerte zu erreichen:
- **Vollmond**: ~50–100 mlx (wolkenlos), ~20–25 mlx (bewölkt) (erreicht: z. B. 99.8 mlx bei Day 12, Hour 23).
- **Sichelmond**: ~1–5 mlx (wolkenlos), ~0.5–1.2 mlx (bewölkt) (erreicht: z. B. 2.5 mlx bei Day 1, Hour 20).
- **Grundhelligkeit**: ~1.6 mlx (wolkenlos), ~0.5 mlx (bewölkt) (erreicht: z. B. 1.6 mlx bei Day 19, Hour 19–02).
- **Tageslicht/Dämmerung**: Werte ≥ 1000 mlx werden ausgeblendet (erreicht: z. B. `*****` bei Day 9, Hour 04).

## Vorteile
- **Physikalische Genauigkeit**: Dynamischer \( \rho \) berücksichtigt die tatsächliche Zenitdistanz des Mondes.
- **Kalibrierung**: Empirisch an reale Daten angepasst, um präzise Millilux-Werte zu liefern.
- **Robustheit**: Sanfte Übergänge über alle Mondphasen und -höhen durch \( \cos(\rho) \)-Streufunktion.
- **Integration**: Kompatibel mit bestehenden Tabellenanzeigen (`renderNightCalcTable`).

## Einschränkungen
- **Zenit-Beobachtung**: Aktuelle Version geht von Beobachtungen am Zenit aus (\( \rho = 90^\circ - \text{moonElevation} \)).
- **Extinktion**: Fester Koeffizient \( k = 0.3 \) mag/airmass, kann für spezifische Bedingungen angepasst werden.
- **Bewölkung**: Einfaches Modell (\( F_{\text{mlx,b}} = F_{\text{mlx,w}} \cdot 0.25 \)), kann für komplexere Szenarien erweitert werden.
- **Datenlücken**: Erfordert vollständige Mondpositions- und -phasen-Daten für alle Stunden.

## Beispielverwendung
```javascript
// Beispiel: Berechnung der Nachthimmelhelligkeit
const time = new Date('2025-04-12T23:00:00Z');
const lat = 48.137154; // Beispiel: München, Breite
const lon = 11.576124; // Beispiel: München, Länge
const moonIllum = { fraction: 1.0 }; // Vollmond
const moonPos = { altitude: 30.90 * Math.PI / 180, azimuth: 0 }; // Mondhöhe in Radiant

const { brightnessW, brightnessB } = calculateSkyBrightness(time, lat, lon, moonIllum, moonPos);
console.log(`BrightnessW: ${brightnessW.toFixed(1)} mlx, BrightnessB: ${brightnessB.toFixed(1)} mlx`);
// Ausgabe: BrightnessW: 99.8 mlx, BrightnessB: 25.0 mlx
```

## Entwicklungsgeschichte
- **Version 1**: Fester \( \rho = 45^\circ \), exponentielle Streufunktion (\( 10^{-0.08 \cdot \rho} \)), Ausgabe in mag/arcsec².
- **Version 2–5**: Dynamischer \( \rho \), verschiedene Streufunktionen (\( 10^{-0.08 \cdot \rho} \), \( 10^{-0.04 \cdot \rho} \)), Ausgabe in mag/arcsec².
- **Version 6**: Dynamischer \( \rho \), \( \cos(\rho) \)-Streufunktion, Ausgabe in mag/arcsec².
- **Finale Version**: Dynamischer \( \rho \), \( \cos(\rho) \)-Streufunktion, Ausgabe in Millilux (mlx), kalibriert an empirische Daten.

## Anmerkungen
- Entwickelt 2025 von Grok (xAI) in Zusammenarbeit mit einem engagierten Benutzer, der durch iterative Tests und Bereitstellung empirischer Daten maßgeblich zur Kalibrierung beigetragen hat.
- Kontakt: Für weitere Anpassungen oder Fragen wende dich an xAI (https://x.ai).