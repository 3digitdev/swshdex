import json
import re

sword = ["Seedot", "Nuzleaf", "Shiftry", "Mawile", "Lunatone", "Scraggy", "Scrafty", "Gothita", "Gothorita", "Gothitelle", "Rufflet", "Braviary", "Deino", "Zweilous", "Hydreigon", "Swirlix", "Slurpuff", "Passimian", "Turtonator", "Jangmo-o", "Hakamo-o", "Kommo-o", "Farfetch'd", "Sirfetch'd", "Darumaka", "Darmanitan", "Flapple", "Stonejourner", "Zacian"]
shield = ["Larvitar", "Pupitar", "Tyranitar", "Lotad", "Lombre", "Ludicolo", "Sableye", "Solrock", "Croagunk", "Toxicroak", "Solosis", "Duosion", "Reuniclus", "Vullaby", "Mandibuzz", "Spritzee", "Aromatisse", "Goomy", "Sliggoo", "Goodra", "Oranguru", "Drampa", "Ponyta", "Rapidash", "Corsola", "Curseola", "Appletun", "Eiscue", "Zamazenta"]

with open("galar_pokedex.json") as gdex:
    armor = {p["name"]: p for p in json.load(gdex)}

with open("pokedex.json") as dex:
    galar = {p["name"]: p for p in json.load(dex)}

full_dex = []

for name, pokemon in galar.items():
    if name in armor:
        num = armor[name]["number"]
        armor_num = num
        if float(num) < 100:
            armor_num = f"0{num}"
        if float(num) < 10:
            armor_num = f"00{num}"
        pokemon["armorNum"] = armor_num
    else:
        pokemon["armorNum"] = ""
    pokemon["exclusive"] = ""
    if name in sword:
        pokemon["exclusive"] = "Sword"
    elif name in shield:
        pokemon["exclusive"] = "Shield"
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
        pokemon["number"] = ""
        pokemon["exclusive"] = ""
        name = re.sub(r'Urshifu (Rapid|Single)\-Strike Style', r'Urshifu (\1)', name)
        pokemon["name"] = name
        full_dex.append(pokemon)

longest = ""
for p in full_dex:
    name = re.sub(r'(Galarian|Alolan) ', r'', p["name"])
    if len(name) > len(longest):
        longest = name

print(longest)

with open("galar_dex.json", "w") as gdex:
    json.dump(full_dex, gdex, indent=4)
