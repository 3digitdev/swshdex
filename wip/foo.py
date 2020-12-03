import json
import re

sword = ["Seedot", "Nuzleaf", "Shiftry", "Mawile", "Lunatone", "Scraggy", "Scrafty", "Gothita", "Gothorita", "Gothitelle", "Rufflet", "Braviary", "Deino", "Zweilous", "Hydreigon", "Swirlix", "Slurpuff", "Passimian", "Turtonator", "Jangmo-o", "Hakamo-o", "Kommo-o", "Farfetch'd", "Sirfetch'd", "Darumaka", "Darmanitan", "Flapple", "Stonejourner", "Zacian"]
shield = ["Larvitar", "Pupitar", "Tyranitar", "Lotad", "Lombre", "Ludicolo", "Sableye", "Solrock", "Croagunk", "Toxicroak", "Solosis", "Duosion", "Reuniclus", "Vullaby", "Mandibuzz", "Spritzee", "Aromatisse", "Goomy", "Sliggoo", "Goodra", "Oranguru", "Drampa", "Ponyta", "Rapidash", "Corsola", "Curseola", "Appletun", "Eiscue", "Zamazenta"]

with open("armor_pokedex.json") as adex:
    armor = {p["name"]: p for p in json.load(adex)}

with open("crown_pokedex.json") as cdex:
    crown = {p["name"]: p for p in json.load(cdex)}

with open("galar_pokedex.json") as dex:
    galar = {p["name"]: p for p in json.load(dex)}

full_dex = []

for name, pokemon in galar.items():
    pokemon["exclusive"] = ""
    if name in sword:
        pokemon["exclusive"] = "Sword"
    elif name in shield:
        pokemon["exclusive"] = "Shield"
    pokemon["armorNum"] = ""
    pokemon["crownNum"] = ""
    full_dex.append(pokemon)

for name, pokemon in armor.items():
    if name not in galar:
        num = pokemon["number"]
        armor_num = num
        if float(num) < 100:
            armor_num = f"0{num}"
        if float(num) < 10:
            armor_num = f"00{num}"
        pokemon["armorNum"] = armor_num
        pokemon["crownNum"] = ""
        pokemon["number"] = ""
        pokemon["exclusive"] = ""
        name = re.sub(r'Urshifu (Rapid|Single)\-Strike Style', r'Urshifu (\1)', name)
        pokemon["name"] = name
        full_dex.append(pokemon)

for name, pokemon in crown.items():
    if name not in galar:
        if pokemon["cdex"] != "":
            num = pokemon["cdex"]
        else:
            num = pokemon["ndex"]
        crown_num = num
        if float(num) < 100:
            crown_num = f"0{num}"
        if float(num) < 10:
            crown_num = f"00{num}"
        pokemon["crownNum"] = crown_num
        pokemon["armorNum"] = ""
        pokemon["number"] = ""
        pokemon["exclusive"] = ""
        full_dex.append({
            "name": pokemon["name"],
            "crownNum": crown_num,
            "armorNum": "",
            "number": "",
            "exclusive": "",
            "pokeType": pokemon["type"]
        })

longest = ""
for p in full_dex:
    name = re.sub(r'(Galarian|Alolan) ', r'', p["name"])
    if len(name) > len(longest):
        longest = name

print(longest)

with open("../public/pokedex.json", "w") as gdex:
    json.dump(full_dex, gdex, indent=4)
