# Dungeons and Dziekans
## Instrukcja uruchomienia
### Systemy debianowe
1. Zainstaluj paczkę `gnu-smalltalk`:
```bash
sudo apt-get install -y gnu-smalltalk
```
2. Uruchom grę używając komendy:
```bash
gst DnD.st
```
3. Graj!

### Uniwersalnie
1. Stwórz obraz Dockera z zainstalowanym `gnu-smalltalk`:
```bash
sudo docker build -t gnu-smalltalk .  
```
2. Uruchom grę używając komendy
```bash
bash gst-run.sh DnD.st
```
3. Graj!