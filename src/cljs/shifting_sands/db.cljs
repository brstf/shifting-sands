(ns shifting-sands.db
  (:require [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [cljs-time.format :as f]
            [cljs-time.core :as time]
            [cljs.reader :as reader]
            [cljs-time.core :as time]
            [re-frame.core :as re-frame]))

(defn list->generate-map
  ([coll] (list->generate-map coll nil))
  ([coll gen-fn]
   (map-indexed
    (fn [idx item]
      (merge {::name item ::index #{(inc idx)}}
             (when gen-fn {::generate-fn gen-fn}))) coll)))

(def spells
  ["Arcane Eye"
   "Auditory Illusion"
   "Beast Form"
   "Befuddle"
   "Bend Fate"
   "Body Swap"
   "Boiling Blast"
   "Charm"
   "Chum Call"
   "Command"
   "Counterspell"
   "Crushing Pressure"
   "Deafen"
   "Dehydrate"
   "Detect Magic"
   "Disguise"
   "Drowning in Words"
   "Earthquake"
   "Elasticity"
   "Elemental Wall"
   "Fog Cloud"
   "Frenzy"
   "Geysers"
   "Gravity Shift"
   "Greed"
   "Gust"
   "Haste"
   "Hatred"
   "Haste"
   "Hover"
   "Icy Touch"
   "Illuminate"
   "Ink Cloud"
   "Inverted Bubble"
   "Knock"
   "Lightning Bolt"
   "Liquid Air"
   "Lodestone"
   "Marble Madness"
   "Masquerade"
   "Miniaturize"
   "Mirror Image"
   "Multiarm"
   "Pacify"
   "Phobia"
   "Giant Growth"
   "Pull"
   "Push"
   "Raise Dead"
   "Raise Spirit"
   "Read Mind"
   "Regeneration"
   "Repel"
   "Scales of Iron"
   "Sculpt Elements"
   "Seafoam Form"
   "Shroud"
   "Shuffle"
   "Sleep"
   "Slug Slime"
   "Sniff"
   "Spellseize"
   "Sudden Wave"
   "Summon Cube"
   "Swarm"
   "Telepathy"
   "Teleport"
   "Thaumaturgic Anchor"
   "Thicket"
   "Time Jump"
   "True Sight"
   "Urchin Barbs"
   "Visual Illusion"
   "Ward"
   "Water Whip"])

(def slug-colors ["White with purple cerata"
                  "Purple with a white stripe"
                  "Tan with brown cerata"
                  "Black with gold scaley cerata"
                  "Pink with translucent cerata"
                  "Teal with red spots"
                  "Gold with black and white stripes"
                  "Translucent white with blue cerata"
                  "Sky blue with dark blue stripes"
                  "Orange with black cerata"
                  "Green with black stripes"
                  "Cream with black speckles"
                  "Gray with pink bumps"
                  "Violet with red cerata"
                  "Yellow with black speckles"
                  "Lime with leaf green cerata"
                  "Blue with yellow cerata"
                  "Indigo with yellow stripes"
                  "Brown with gray bumps"
                  "Red with maroon spots"])

(def slug-effects (concat (repeat 5 "Regeneration")
                          ["Miniaturize"
                           "Multiarm"
                           "Slug Slime"
                           "Scales of Iron"
                           "Hover"
                           "Haste"
                           "Command"
                           "Deafen"
                           "Charm"
                           "Frenzy"
                           "Hatred"
                           "Hypnotize"
                           "Pacify"
                           "Sleep"
                           "Thaumaturgic Anchor"]))

(def coral ["Soothing Coral"
            "Chalky Coral"
            "Lantern Coral"
            "Sea Pen"
            "Elkhorn Coral"
            "Pillar Coral"
            "Bulb Coral"
            "Pike Coral"
            "Brain Coral"
            "Fire Coral"
            "Urchin's Spine"
            "Calcium Coral"])

(def general-gear1 ["2x Torch"
                    "Bear Trap"
                    "Shovel"
                    "Bellows"
                    "Grease"
                    "Saw"
                    "Bucket"
                    "Caltrops"
                    "Chisel"
                    "Drill"
                    "Fishing Rod"
                    "Marbles"
                    "Glue"
                    "Pick"
                    "Hourglass"
                    "Net"
                    "Tongs"
                    "Lockpick"
                    "Metal file"
                    "{{melee-weapon}}"])

(def general-gear2 ["2x Torch"
                    "Sponge"
                    "Lens"
                    "Perfume"
                    "Horn"
                    "Bottle"
                    "Soap"
                    "Spyglass"
                    "Tar Pot"
                    "Twine"
                    "Fake Jewels"
                    "Card Deck"
                    "Dice Set"
                    "Face Paint"
                    "Whistle"
                    "Instrument"
                    "Quill & Ink"
                    "Small Bell"
                    "Airbladder"
                    "{{ranged-weapon}}"])

(def physique ["Gaunt" "Scrawny" "Slender" "Wiry" "Flabby" "Average"
               "Average" "Athletic" "Stout" "Brawny" "Ripped" "Hulking"])

(def face ["Blunt" "Bony" "Chiseled" "Delicate" "Elongated" "Pinched"
           "Hawkish" "Ratlike" "Round" "Sunken" "Square" "Wolfish"])

(def hair ["Bald" "Braided" "Bristly" "Cropped" "Curly" "Disheveled"
           "Dreadlocks" "Filthy" "Frizzy" "Greased" "Limp" "Long"
           "Luxurious" "Mohawk" "Oily" "Ponytail" "Silky" "Topknot"
           "Wavy" "Wispy"])

(def height (concat (repeat 2 "Tiny")
                    (repeat 2 "Short")
                    (repeat 4 "Average")
                    (repeat 2 "Tall")
                    (repeat 2 "Towering")))

(def speech ["Blunt" "Booming" "Breathy" "Cryptic" "Droning" "Flowery"
             "Formal" "Gravelly" "Mumbling" "Quaint" "Squeaky" "Whispery"])

(def clothing ["Antique" "Ceremonial" "Decorated" "Eccentric" "Elegant"
               "Fashionable" "Filthy" "Stained" "Frayed" "Shabby" "Patched"
               "Simple"])

(def passions ["Lazy (N/A)" "Athletics (STR)" "Acrobatics (DEX)"
               "Hard Work (CON)" "Learning (INT)" "Experience (WIS)"
               "Socializing (CHA)" "Martial Arts (STR)" "Craftsmanship (DEX)"
               "Cooking (CON)" "Magic (INT)" "Spirituality (WIS)"
               "Music (CHA)" "Swimming (STR)" "Travel (CON)" "Dance (DEX)"
               "Science (INT)" "Marksmanship (WIS)" "Art (CHA)"
               "Prodigy (Pick Two)"])

(def a-mana-me ["Fire" "Water" "Electric" "Poison"])

(def treasure ["An anchor with an unusually long shank"
               "A vibrantly colored conch"
               (str "A ship's wheel that has been retrofitted with a "
                    "backing and handle")
               (str "Horn made of tarnished brass that has the image "
                    "of a lighthouse inscribed on it")
               (str "A large crab's claw that has been strung with a "
                    "fine thread")
               "It looks somewhat like an ornate brass astrolabe"
               "A large, two-handed, tree felling axe"
               "A massive maul made of granite"
               "A scythe that's blade is mostly transparent"
               "A pick with a translucent head"
               "A shovel with a head made of glass"
               (str "A harpoon, the point appears to be made of "
                    "volcanic rock and is split down the middle")])

(def universal-rooms
  {::the-plunge
   {
    ::name "The Plunge"
    ::hallways [:east :south :west]
    ::danger 0
    ::id 0
    }
   ::octopod-workshop
   {
    ::name "Octopod Workshop"
    ::num-hallways 1
    ::danger 0
    }
   ::the-plunge-reef
   {
    ::name "The Plunge"
    ::hallways [:south]
    ::danger 0
    ::id 0
    }
   ::the-great-barrier
   {
    ::name "The Great Barrier"
    ::hallways [:north :south]
    ::danger -1
    ::id 1
    }
   ::unexplored
   {
    ::name "Unexplored"
    ::id ::unexplored
    }
   ::empty
   {
    ::name "Empty"
    ::id ::empty
    }
   ::exit
   {
    ::name "Exit"
    ::id ::exit
    ::danger 0
    ::num-hallways 1
    }
   })

(def tables
  {::room
   {::pelagic
    [
     {
      ::name "Hanging Gardens"
      ::description (str "{{1d4[+]}} Hermits are selling\n- 250s {{slug}}\n"
                         "- 250s {{slug}}\n- 250s {{slug}}\n")
      ::num-hallways 2
      ::danger -2
      ::index #{1}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Open Water"
      ::description "{{open-water-pelagic}}"
      ::num-hallways 4
      ::danger 0
      ::index #{2}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "The Currents"
      ::num-hallways 3
      ::danger -1
      ::index #{3}
      }
     {
      ::name "Sinking Ship"
      ::description "Trap - {{trap}}\nInside the ship: {{sinking-ship}}"
      ::num-hallways 2
      ::danger 0
      ::index #{4}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Whirlpool Well"
      ::num-hallways 4
      ::danger -1
      ::index #{5}
      }
     {
      ::name "Seagrass Plains"
      ::description (str "{{1d4:1:The Chest is a Mimic Fish!;2-4:The "
                         "chest contains treasure!}}")
      ::num-hallways 4
      ::danger 1
      ::index #{6}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Spawning Pool"
      ::description (str "{{1d4[-]:1:A Chum;2:3 Chum;3:3 Chum and a Chum "
                         "Guard with {{armor}};4:4 Chum, a Chum Guard "
                         "with {{armor}} and another with {{armor}} "
                         "and a Chum Priest with {{spell}}}}")
      ::num-hallways 2
      ::danger 0
      ::index #{7}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Driftwood Pile"
      ::description (str "What's in the Pile?\n{{1d8:1:A Wandering Hermit "
                         "Merchant:{{shop}};2:2 Chum;3:3 Glaucimmian;"
                         "4:4 Gulltures;5:6 Chum;6:An Eelaconda;"
                         "7:8 Chum;8:A Scale Stalker}}")
      ::num-hallways 1
      ::danger 1
      ::index #{8}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Gullture Eerie"
      ::description "Trap - {{trap}}"
      ::num-hallways 3
      ::danger 1
      ::index #{9}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Chum Nest"
      ::description (str "Trap - {{trap}}\nChum that live here:{{1d4:"
                         "1:3 Chum;2:5 Chum;3:4 Chum and a Chum Guard with "
                         "{{armor}};4:4 Chum, a Chum Guard with {{armor}} "
                         "and another with {{armor}} and a Chum Priest "
                         "with {{spell}}}}")
      
      ::num-hallways 3
      ::danger -1
      ::index #{10}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Upside Down"
      ::description (str "Trap - {{trap}}\nHidden slugs:\n- {{slug}}\n"
                         "- {{slug}}\n- {{slug}}\n- {{slug}}")
      ::num-hallways 3
      ::danger 1
      ::index #{11}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "The Shelf"
      ::description "{{twilight-encounter}}"
      ::num-hallways 3
      ::danger 2
      ::index #{12}
      ::generate-fn ::fill-str-templates
      }
     ]
    ::the-reef
    [
     {
      ::name "Golem's Glade"
      ::num-hallways 2
      ::danger -1
      ::index #{1}
      }
     {
      ::name "Thicket"
      ::description "Trap - {{trap}}\nThere's also one Reef Golem here"
      ::num-hallways 4
      ::danger 0
      ::index #{2}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "The Speaker"
      ::num-hallways 4
      ::danger 0
      ::index #{3}
      }
     {
      ::name "Elevator"
      ::num-hallways 1
      ::danger 0
      ::index #{4}
      }
     {
      ::name "A-Mana-Me Garden"
      ::num-hallways 1
      ::danger -1
      ::index #{5}
      }
     {
      ::name "Coral Maze"
      ::description (str
                     "Trap - {{trap}}. Size of Halls: {{1d6:"
                     "1:Open - The Heirs can pass through easily;"
                     "2:Tight - Two by two only;"
                     "3:Low - Heirs must bend over and move at half speed;"
                     "4:Fat man's squeeze - Single file;"
                     "5:Stifling - It's dark, no light;"
                     "6:Time to crawl - Heirs must crawl, quarter speed}}")
      ::num-hallways "{{1d4}}"
      ::danger 0
      ::index #{6 7 8 9}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Spinney"
      ::description (str "Type of coral: {{1d4:1:Pike Coral;2:Fire Coral;"
                         "3:Urchin's Spine;4:Calcium Coral}}")
      ::num-hallways 3
      ::danger 1
      ::index #{10 11}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Lagoon Arena"
      ::num-hallways 4
      ::danger 0
      ::index #{12}
      }]
    ::the-kelp-forest
    [
     {
      ::name "Weed Mat"
      ::description (str "{{1d8:1-4:There are signs of dotters living here, "
                         "but none are present, they must be roaming "
                         "elsewhere on the floor;5-8:{{n}} dotters here! "
                         "They are cautious but curious}}")
      ::num-hallways 4
      ::danger -1
      ::index #{1}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Salty Orchard"
      ::description "Herd of {{1d8}} Sea Cows\n{{1d6}} Glaucimmian"
      ::num-hallways 3
      ::danger 0
      ::index #{2}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "The Weeper"
      ::num-hallways 2
      ::num-secret-hallways 1
      ::danger 1
      ::index #{3}
      }
     {
      ::name "Kelp Copse"
      ::num-hallways "{{1d4}}"
      ::danger 1
      ::index #{4 5 6}
      }
     {
      ::name "Tangleweed Thicket"
      ::num-hallways 2
      ::danger 0
      ::index #{7 8}
      }
     {
      ::name "Chum Nest"
      ::description (str "{{1d4:1:8 Chum;2:12 Chum;3:10 Chum, a Chum Guard "
                         "with {{armor}}, and another with {{armor}};4:12 "
                         "Chum, a Chum Guard with {{armor}}, another with "
                         "{{armor}}, and a Chum Priest with {{spell}}}}")
      ::num-hallways 3
      ::danger 0
      ::index #{9}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Urchin Barren"
      ::num-hallways 4
      ::danger -1
      ::index #{10}
      }
     {
      ::name "Sunken Table"
      ::num-hallways 1
      ::danger 2
      ::index #{11}
      }
     {
      ::name "The Chimney"
      ::num-hallways 1
      ::danger 1
      ::index #{12}
      }]
    ::the-twilight
    [{
      ::name "Elevator"
      ::num-hallways 1
      ::danger 0
      ::index #{1}
      }
     {
      ::name "The Nest"
      ::num-hallways 3
      ::danger -1
      ::index #{2}
      }
     {
      ::name "Open Water"
      ::description (str "Floating in the channels: {{1d6:1:4 Chum float "
                         "through, they are completely lost;2:A sack of "
                         "Loot floats past with:\n- {{loot}}\n- {{loot}}"
                         "\n- {{loot}};3-4:Someone sent the dead's sand "
                         "down. Gain 1000 sand;5:A dead Chum floating on "
                         "its back, it's holding an onyx figurine. The "
                         "figurine acts as a Spellpearl with {{spell}} "
                         "and {{spell}};6:2 Electric Eelaconda enter}}")
      ::num-hallways 4
      ::danger 1
      ::index #{3 4}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Spawning Pool"
      ::description (str "Distracted Chum: {{1d4:1:4 Chum and 2 Balloon "
                         "Chum;2:6 Tropical Chum;3:6 Chumacuda;4:4 Deep "
                         "Chum}}")
      ::num-hallways 2
      ::danger 1
      ::index #{5 6}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Chum Town"
      ::description (str "{{1d6:1:10 Chum;2:8 Chum and a Chum Priest with "
                         "{{spell}};3:6 Chum and an Eelaconda;4:12 Chum, "
                         "a Chum Priest with {{spell}}, and another with "
                         "{{spell}};5:6 Chum Priests are chanting, more "
                         "Chum watch curiously from the channels in the "
                         "walls. The priests have {{spell}}, {{spell}}, "
                         "{{spell}}, {{spell}}, {{spell}}, and {{spell}};"
                         "6:A Deep Chum champion (wearing coral plate and "
                         "wielding a random treasure) riding a Hammerhead "
                         "Stalker}}")
      ::num-hallways 4
      ::danger 0
      ::index #{7 8}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Whirlpool Well"
      ::num-hallways 4
      ::danger -1
      ::index #{9}
      }
     {
      ::name "Blue"
      ::num-hallways 4
      ::danger 2
      ::index #{10 11}
      }
     {
      ::name "Shelf"
      ::num-hallways 3
      ::danger 2
      ::index #{12}
      }]
    ::the-trench
    [{
      ::name "The Wheel"
      ::num-hallways 2
      ::danger 0
      ::index #{1}
      }
     {
      ::name "A Ravine"
      ::num-hallways 2
      ::danger 1
      ::index #{2 3 4}
      }
     {
      ::name "Vent Puzzle"
      ::num-hallways 2
      ::danger 0
      ::index #{5}
      }
     {
      ::name "Hall of Vents"
      ::num-hallways 2
      ::danger -1
      ::index #{6 7 8}
      }
     {
      ::name "Dolpod Temple"
      ::description (str "{{1d4:1:4 Dolpods;2:6 Dolpods;3:8 Dolpods;"
                         "4:A Dolpod Hive (Dolpod with 2x HP and screeches "
                         "that deal 1d8 Psychic Damage}}")
      ::num-hallways 2
      ::danger -2
      ::index #{9}
      ::generate-fn ::fill-str-templates
      }
     {
      ::name "Worm Hedge"
      ::num-hallways 2
      ::danger 0
      ::index #{10 11}
      }
     {
      ::name "Yeti Mounds"
      ::num-hallways 3
      ::danger -1
      ::index #{12}
      }]
    }
   ::trap
   {
    ::pelagic
    [{
      ::name "No Trap"
      ::index #{1 2 3}
      }
     {
      ::name "Vel Vines"
      ::index #{4}
      }
     {
      ::name "Porps"
      ::index #{5 6}
      }
     {
      ::name "Channel Flush"
      ::index #{7 8}
      }
     {
      ::name "Nabbit Worm"
      ::index #{9}
      }
     {
      ::name "Mimic Fish"
      ::index #{10}
      }
     {
      ::name "Lady o' Peace"
      ::index #{11 12}
      }]
    ::the-reef
    [{::name "No Trap"
      ::index #{1 2}}
     {::name "Table Coral False Floor. pit trap, 10ft. deep"
      ::index #{3}}
     {::name "Porps"
      ::index #{4}}
     {::name "Plague Urchins"
      ::index #{5}}
     {::name "Pit of Pike Coral. DEX Save or take 1d6 Piercing"
      ::index #{6}}
     {::name "Fire Coral Wall. CON Save or take 1d6 Heat"
      ::index #{7}}
     {::name "Channel Flush. STR Save or get pushed to next room"
      ::index #{8}}
     {::name "Nabbit Worm"
      ::index #{9}}
     {::name "A-mana-me"
      ::index #{10}}
     {::name "Mimic Fish"
      ::index #{11}}
     {::name "Doorgler"
      ::index #{12}}]
    }
   ::situation
   [{::name "It's Empty"
     ::index #{1 2 3 4 5}}
    {::name "Loot!"
     ::description "The heirs find:\n{{situation-loot}}"
     ::index #{6}
     ::generate-fn ::fill-str-templates}
    {::name "Open for Business!"
     ::description "{{shop-inventory}}"
     ::index #{7}
     ::generate-fn ::fill-str-templates}
    {::name "A Secret..."
     ::description "Spellpearl - {{spell}}"
     ::index #{8}
     ::generate-fn ::fill-str-templates}
    {::name "The Pits"
     ::index #{9 10}}
    {::name "A Deep Effigy"
     ::index #{11}}
    {::name "Feeding Frenzy"
     ::description "{{encounter++}}"
     ::index #{12}
     ::generate-fn ::fill-str-templates}
    {::name "Sunken Shrine to the {{god}}"
     ::index #{13}
     ::generate-fn ::fill-name-templates}
    {::name "Rushing Water"
     ::index #{14 15}}
    {::name "Oh No, a Deep Pool"
     ::description "{{deep-pool-attribute}}"
     ::index #{16}
     ::generate-fn ::fill-str-templates}
    {::name "A Hunt is Afoot"
     ::description "{{encounter+}}\nis hunting\n{{encounter-}}"
     ::index #{17}
     ::generate-fn ::fill-str-templates}
    {::name "We're Not Alone"
     ::description "The Heirs are stalked by:\n{{encounter++}}"
     ::index #{18}
     ::generate-fn ::fill-str-templates}
    {::name "Rip Current"
     ::index #{19}}
    {::name "A Rogue Wave"
     ::description "Inside the wave:\n{{rogue-wave-contents}}"
     ::index #{20}
     ::generate-fn ::fill-str-templates}]
   ::deep-pool-attribute
   [{::description "The water is full of Mystic Remora"
     ::index #{1}}
    {::description "The water is fairly clear"
     ::index #{2 3}}
    {::description "The water is murky"
     ::index #{4}}]
   ::god
   [{::name "Yellow Princess"
     ::index #{1}}
    {::name "High Lady of Frigil"
     ::index #{2}}
    {::name "High Lord of Foltoran"
     ::index #{3}}
    {::name "High Lord of Elcontra"
     ::index #{4}}
    {::name "Green Prince"
     ::index #{5}}
    {::name "Deep Queen"
     ::index #{6}}]
   ::rogue-wave-contents
   [{::description (str "A chest! Contains:\n{{loot}}\n{{loot}}\n"
                        "{{loot}}\n{{loot}}")
     ::index #{1}
     ::generate-fn ::fill-str-templates}
    {::description "A dead sailor. Has {{loot}}"
     ::index #{2}
     ::generate-fn ::fill-str-templates}
    {::description (str "{{depthd6}} Chum come crashing into the "
                        "room with the wave")
     ::index #{3}
     ::generate-fn ::fill-str-templates}
    {::description (str "Some discarded netting floats along. Any creature "
                        "in the wave must make a DEX Save or become "
                        "tangled. If they become tangled they must make "
                        "a CON Save or drown and die")
     ::index #{4}}
    {::description (str "{{1d6}} Dolpods swimming easily inside the wave. "
                        "They attack any Heir that is caught in the wave "
                        "with them.")
     ::index #{5}
     ::generate-fn ::fill-str-templates}
    {::description (str "A Spear Swimmer which attempts to impale any Heirs "
                        "caught in the wave and carry them with the wave")
     ::index #{6}}]
   ::encounter
   {
    ::pelagic
    [
     {
      ::description "The Heirs find a Clam. Inside is:\n{{loot}}"
      ::index #{1}
      ::generate-fn ::fill-str-templates
      }
     {
      ::description (str "A flock of four Chel fly into the room and begin"
                         " munching on vegetation.")
      ::index #{2}
      }
     {
      ::description "A lone Chum comes obliviously walking into the room"
      ::index #{3}
      }
     {
      ::description "3 Glaucs (Glaucimmian) climb into the room"
      ::index #{4}
      }
     {
      ::description "3 Chum walk into the room carelessly."
      ::index #{5}
      }
     {
      ::description (str "An Octopod rides on the back of its walker "
                         "into the room. It worriedly looks around "
                         "for Cuttlers, and offers to show the Heirs "
                         "to a safe place. If the Heirs follow, the "
                         "Octopod shows them its Workshop")
      ::index #{6}
      }
     {
      ::description (str "A Hermit crawls by. It stops and attempts to "
                         "communicate with the Heirs to sell them:\n"
                         "{{shop-item}}")
      ::index #{7}
      }
     {
      ::description "6 Gulltures stalk into the room."
      ::index #{8}
      }
     {
      ::description "6 Chum enter, one of them is a Priest with {{spell}}"
      ::index #{9}
      ::generate-fn ::fill-str-templates
      }
     {
      ::description "A pack of 6 Dotters scamper through the room"
      ::index #{10}
      }
     {
      ::description "2 Dolpods come into the room stealthily."
      ::index #{11}
      }
     {
      ::description (str "8 Chum enter. Half of them have spears and the "
                         "other half have slings")
      ::index #{12}
      }
     {
      ::description "An Eelaconda begins stalking the Heirs."
      ::index #{13}
      }
     {
      ::description (str "8 Glaucs come tearing into the room, screaming "
                         "while being chased by an Eelaconda")
      ::index #{14}
      }
     {
      ::description "A lone Cuttler begins to stalk the Heirs"
      ::index #{15}
      }
     {
      ::description (str "8 Chum appear with spears and slings. One "
                         "is a Priest with {{spell}}")
      ::index #{16}
      ::generate-fn ::fill-str-templates
      }
     {
      ::description "A Shrimp Swarm comes buzzing into the room"
      ::index #{17}
      }
     {
      ::description (str "A Scale Stalker is hunting the Heirs. It has "
                         "caught them")
      ::index #{18}
      }
     {
      ::description "A Sea Swallower enters the far end of the room."
      ::index #{19}
      }
     {
      ::description "Two twin Scale Stalkers enter the room."
      ::index #{20}}
     ]
    ::the-reef
    [{::description (str "3 Rations worth of Not-So-Sea Cucumbers slide "
                         "about the room and munch on organics they "
                         "come upon. Slimey chewy goodness")
      ::index #{1}}
     {::description (str "A flock of Chel fly into the room and begin "
                         "munching on vegetation")
      ::index #{2}}
     {::description (str "2 Tropical Chum saunter into the room. They "
                         "know they look fabulous")
      ::index #{3}}
     {::description "3 Balloon Chum waddle into the room"
      ::index #{4}}
     {::description "A Brain Coral sits in here all alone.."
      ::index #{5}}
     {::description (str "An Octopod rides on the back of its walker "
                         "into the room. It worriedly looks around "
                         "for Cuttlers, and offers to show the Heirs "
                         "to a safe place. If the Heirs follow, the "
                         "Octopod shows them its Workshop")
      ::index #{6}}
     {::description (str "A Hermit crawls by. It stops and attempts to "
                         "communicate with the Heirs to sell them:\n"
                         "{{shop-item}}\n{{shop-item}}")
      ::index #{7}
      ::generate-fn ::fill-str-templates}
     {::description (str "A school of 7 Tropical Chum are in a hunting "
                         "party. They are armed with Urchin Quill "
                         "blowguns and Coral Pikes")
      ::index #{8}}
     {::description (str "A Scale Stalker has picked up the Heirs motion in "
                         "the channels. It stalks them silently")
      ::index #{9}}
     {::description (str "A Mystic Remora is drawn to magical items. "
                         "It attempts to sneak into the pack of an Heir "
                         "containing a magical item and suck the Sand "
                         "from the item.")
      ::index #{10}}
     {::description (str "A Steelbeak is munching a suit of armor in "
                         "here, and maybe the corpse inside.")
      ::index #{11}}
     {::description (str "8 Tropical Chum and 2 Balloon Chum come marching "
                         "into the room. One of the Tropical Chum is "
                         "wearing a headdress of coral and carries a "
                         "Spell Pearl with {{spell}}. The Tropical Chum "
                         "are armed with Urchin Quill Blowguns and Coral "
                         "Pikes")
      ::index #{12}
      ::generate-fn ::fill-str-templates}
     {::description "2 Steelbeaks come into the room sniffing down metal"
      ::index #{13}}
     {::description "A Rainbow Shrimp charges the Heirs"
      ::index #{14}}
     {::description "A lone Cuttler begins to stalk the Heirs"
      ::index #{15}}
     {::description "A lone bull Reef Golem comes crashing into the room"
      ::index #{16}}
     {::description (str "4 Dolpods attack the Heirs. One has a Spellpearl "
                         "with {{spell}}. They're armed with\n"
                         "{{melee-weapon}}\n{{melee-weapon}}\n"
                         "{{melee-weapon}}\n{{melee-weapon}}\n")
      ::index #{17}
      ::generate-fn ::fill-str-templates}
     {::description (str "A Shrimp Swarm comes buzzing into the room. "
                         "It attacks the smallest creature and moves up "
                         "to the largest, after sucking the last victim "
                         "dry")
      ::index #{18}}
     {::description "A Sea Swallower enters the far end of the room"
      ::index #{19}}
     {::description (str "A pair of young aggressive Reef Golems enter. "
                         "They are engorged with coral spores and looking "
                         "for a place to create their own glade")
      ::index #{20}}]
    ::the-kelp-forest
    [{::description (str "3 Rations worth of Not-So-Sea Cucumbers slide "
                         "about the room and munch on organics they "
                         "come upon. Slimey chewy goodness")
      ::index #{1}}
     {::description "The Heirs find a Clam. Inside is:\n{{loot}}"
      ::index #{2}
      ::generate-fn ::fill-str-templates}
     {::description (str "A couple of Chels enter. Two Chels fly into "
                         "the room and begin munching on vegetation. "
                         "They will fly away if startled")
      ::index #{3}}
     {::description (str "A lone Hermit crawls into view. Their shell is "
                         "covered in anemone and coral growths. They are "
                         "VERY proud of their shell and will talk about it "
                         "endlessly. Their name is Alkr'flk.")
      ::index #{4}}
     {::description (str "An Octopod rides on the back of its walker "
                         "into the room. It worriedly looks around "
                         "for Cuttlers, and offers to show the Heirs "
                         "to a safe place. If the Heirs follow, the "
                         "Octopod shows them its Workshop")
      ::index #{5}}
     {::description (str "A pack of 8 Dotters come chirping and yipping "
                         "into the room. They playfully run around the "
                         "Heirs. A couple are young, and could be tamed. "
                         "It takes 4 Rations and a CHA Save to successfully "
                         "tame a Dotter")
      ::index #{6}}
     {::description (str "4 Skipper Chum hop into combat and can dive onto "
                         "opponents")
      ::index #{7}}
     {::description "8 Plague Urchins crawl slowly around the room"
      ::index #{8}}
     {::description "4 Rockskipper Chujm and two Balloon Chum enter the room"
      ::index #{9}}
     {::description "A small contingent of 4 Dolpods walk into the room."
      ::index #{10}}
     {::description "A Cannon Shrimp recklessly attacks!"
      ::index #{11}}
     {::description (str "6 Rockskipper Chum enter, and one of them has "
                         "a Spellpearl with {{spell}}")
      ::index #{12}
      ::generate-fn ::fill-str-templates}
     {::description "2 Scale Stalkers stalk into the room"
      ::index #{13}}
     {::description "8 Rockskipper Chum enter the room"
      ::index #{14}}
     {::description "A Cuttler enters the room and begins stalking an Heir"
      ::index #{15}}
     {::description "An Electric Eelaconda has a nest here"
      ::index #{16}}
     {::description "A Doorgler is hiding in one of the halls of this room"
      ::index #{17}}
     {::description "4 Chumacuda enter the room"
      ::index #{18}}
     {::description (str "2 Cannon Shrimp and 2 Rainbow Shrimp are "
                         "sparring in this room. They turn their "
                         "attention to the Heirs")
      ::index #{19}}
     {::description "A Hammer Scale Stalker is in the room"
      ::index #{20}}]
    ::the-twilight
    [{::description "The Heirs find a Clam. Inside is:\n{{loot}}"
      ::index #{1}
      ::generate-fn ::fill-str-templates}
     {::description "The room is filled with Glow Jellies"
      ::index #{2}}
     {::description "A school of 4 Deep Chum skulk in the room"
      ::index #{3}}
     {::description "A swarm of Firefly Squid blink rhythmically"
      ::index #{4}}
     {::description (str "A lone Hermit crawls into view. Their shell is "
                         "covered in anemone and coral growths. They are "
                         "VERY proud of their shell and will talk about it "
                         "endlessly. Their name is Alkr'flk.")
      ::index #{5}}
     {::description (str "2 Rainbow Shrimp box here. They attack the heir "
                         "with the most magic items")
      ::index #{6}}
     {::description (str "A pod of 6 Dolpods fanatically worship in "
                         "the twilight. They are in a religious fervor")
      ::index #{7}}
     {::description "A Spear Swimmer streaks into the room after the heirs"
      ::index #{8}}
     {::description (str "A Scale Stalker sits staring at the channels "
                         "as the Heirs enter")
      ::index #{9}}
     {::description (str "A pod of 8 Dolpods fanatically scream "
                         "telepathically in this room. The telepathic "
                         "cacophony requires a WIS Save or take 2d6 "
                         "Psychic Damage. They are preoccupied")
      ::index #{10}}
     {::description "3 Yeti Crabs graze territorially"
      ::index #{11}}
     {::description (str "A pair of Electric Eelaconda enter and begin "
                         "to stalk the Heirs")
      ::index #{12}}
     {::description (str "A pod of 6 Dolpods enter, one is wearing "
                         "priests vestement and carries two Spellpearls: "
                         "{{spell}} and {{spell}}")
      ::index #{13}
      ::generate-fn ::fill-str-templates}
     {::description (str "A Hammer Scale Stalker is chained to the floor "
                         "here. It rages at any creatures that enter")
      ::index #{14}}
     {::description "A Vampire Squid begins stalking the Heirs"
      ::index #{15}}
     {::description "A Marionetta Squid works its puppet here"
      ::index #{16}}
     {::description "A Doorgler is hiding in one of the halls of this room"
      ::index #{17}}
     {::description "A school of 5 Chumacuda enter the room"
      ::index #{18}}
     {::description (str "A pod of 6 Dolpods begin hunting the Heirs. They "
                         "don't know one of them is being controlled by a "
                         "Marionetta Squid. They know where the Heirs are")
      ::index #{19}}
     {::description "The mightly Sperm Behemoth slams into the room"
      ::index #{20}}]
    ::the-trench
    [{::description "The Heirs find a clam! Inside is a Treasure!"
      ::index #{1}}
     {::description "The room is filled with Glow Jellies"
      ::index #{2}}
     {::description "A school of 4 Deep Chum skulk in the room"
      ::index #{3}}
     {::description "A swarm of Firefly Squid blink rhythmicly"
      ::index #{4}}
     {::description "A school of 6 Deep Chum come into the room"
      ::index #{5}}
     {::description "2 Spear Swimmer careen into the room"
      ::index #{6}}
     {::description "4 Scale Stalkers enter the room"
      ::index #{7}}
     {::description "A small pod of 6 Dolpods enter the room"
      ::index #{8}}
     {::description "2 Marionetta Squid put on a morbid play here"
      ::index #{9}}
     {::description (str "A pod of 8 Dolpods fanatically scream "
                         "telepathically in this room. The telepathic "
                         "cacophony requires a WIS Save or take 2d6 "
                         "Psychic Damage. They are preoccupied")
      ::index #{10}}
     {::description (str "4 Electric Eelaconda enters and beings to "
                         "stalk the Heirs")
      ::index #{11}}
     {::description (str "8 Deep Chum enter; one is wearing Coral Armor and "
                         "wielding a Treasure (roll on the Treasure table. "
                         "Another has two Spellpearls with {{spell}} and "
                         "{{spell}}")
      ::index #{12}}
     {::description "A Vampire Squid stalks the Heirs"
      ::index #{13}}
     {::description "12 Deep Chum enter the room"
      ::index #{14}}
     {::description (str "A pair of Cuttler twins enter, and begin stalking "
                         "an Heir")
      ::index #{15}}
     {::description "A Doorgler is hiding in one of the halls of this room"
      ::index #{16}}
     {::description "5 Yeti Crabs scuttle by"
      ::index #{17}}
     {::description (str "A pod of 8 sneaky Dolpods begin hunting the Heirs. "
                         "They know where the Heirs are.")
      ::index #{18}}
     {::description "A Kraken storms into the room"
      ::index #{19}}
     {::description "A Sperm Behemoth is fighting a Kraken in this room"
      ::index #{20}}]
    }
   ::loot
   [
    {
     ::index #{1}
     ::description "2x rations worth of pickled herring in a jar."
     }
    {
     ::index #{2}
     ::description "{{1d4[+]}} rations worth of dried fish. It's salty."
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{3}
     ::description "A bundle of {{1d6}} torches. They burn for three Rooms each"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{4}
     ::description "2x Glass Jars. They're big enough for Slugs!"
     }
    {
     ::index #{5}
     ::description "2x Glass Buoys. They float and look pretty."
     }
    {
     ::index #{6}
     ::description (str "A weighted net. Can hold a human-sized creature. "
                        "It's got rocks to hold it down.")
     }
    {
     ::index #{7}
     ::description (str "A vial of Urchin venom (two doses). CON Save "
                        "or take 1 Poison Damage per turn.")
     }
    {
     ::index #{8}
     ::description "A slug in a Glass Jar. The slug is {{slug}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{9}
     ::description "A scroll made of kelp - {{spell}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{10}
     ::description "A rusty {{rusty-melee-weapon}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{11}
     ::description (str "Fishing rod and tackle. Can spend a rest fishing. "
                        "Roll 1d6[-] gain that many rations worth of fish.")
     }
    {
     ::index #{12}
     ::description (str "Some handy equipment:\n"
                        "- {{dungeoneering-gear}}\n"
                        "- {{dungeoneering-gear}}")
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{13}
     ::description "2 scrolls made of kelp - {{spell}} and {{spell}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{14}
     ::description (str "2 Slugs in 2 Glass Jars. One slug is {{slug}}"
                        " the other is {{slug}}")
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{15}
     ::description "A used suit of armor: {{used-armor}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{16}
     ::description "A used {{used-ranged-weapon}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{17}
     ::description "A Spell Pearl - {{spell}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{18}
     ::description (str "A couple of Slugs in Glass Jars. One is {{slug}} "
                        "the other is {{slug}}")
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{19}
     ::description "A minor magical {{magic-melee-weapon}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{20}
     ::description "Some magical treasure! How lucky! Roll on the Treasure table."
     }]
   ::dungeoneering-gear
   [
    {
     ::index #{1}
     ::description "Rope, 50ft"
     }
    {
     ::index #{2}
     ::description "Pulleys"
     }
    {
     ::index #{3}
     ::description "Chain, 10ft"
     }
    {
     ::index #{4}
     ::description "Chalk, 10x"
     }
    {
     ::index #{5}
     ::description "Crowbar"
     }
    {
     ::index #{6}
     ::description "Torch, 2x (light for 3 rooms)"
     }
    {
     ::index #{7}
     ::description "Grappling Hook"
     }
    {
     ::index #{8}
     ::description "Hammer"
     }
    {
     ::index #{9}
     ::description "Padlock"
     }
    {
     ::index #{10}
     ::description "Manacles"
     }
    {
     ::index #{11}
     ::description "Mirror"
     }
    {
     ::index #{12}
     ::description "Pole, 10ft"
     }
    {
     ::index #{13}
     ::description "Sack"
     }
    {
     ::index #{14}
     ::description "Machete"
     }
    {
     ::index #{15}
     ::description "Spikes, 5"
     }
    {
     ::index #{16}
     ::description "Scroll - {{spell}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{17}
     ::description "Lantern and Blubber Oil (light for 1 floor)"
     }
    {
     ::index #{18}
     ::description "Glowing Algae Globe"
     }
    {
     ::index #{19}
     ::description "Slug - {{slug}}"
     ::generate-fn ::fill-str-templates
     }
    {
     ::index #{20}
     ::description "Spellpearl - {{spell}}"
     ::generate-fn ::fill-str-templates
     }]
   ::melee-weapon
   [{::index #{1 2 3 4}
     ::description {::name "Dagger"
                    ::damage "1d6 Pierce"
                    ::slot 1
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{5 6}
     ::description {::name "Cudgel"
                    ::damage "1d6 Bludgeon"
                    ::slot 1
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{7}
     ::description {::name "Sickle"
                    ::damage "1d6 Slash"
                    ::slot 1
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{8 9}
     ::description {::name "Mace"
                    ::damage "1d8 Bludgeon"
                    ::slot 2
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{10 11 12 13}
     ::description {::name "Spear"
                    ::damage "1d8 Pierce"
                    ::slot 2
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{14}
     ::description {::name "Hand Ax/Sword"
                    ::damage "1d8 Slash"
                    ::slot 2
                    ::hand 1
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{15}
     ::description {::name "Eku"
                    ::damage "1d8 Slash or Bludgeon"
                    ::slot 3
                    ::hand 2
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{16}
     ::description {::name "Hammer"
                    ::damage "1d10 Bludgeon"
                    ::slot 3
                    ::hand 2
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{17 18}
     ::description {::name "Harpoon"
                    ::damage "1d10 Pierce"
                    ::slot 3
                    ::hand 2
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{19}
     ::description {::name "Ax/Longsword"
                    ::damage "1d10 Slash"
                    ::slot 2
                    ::hand 2
                    ::quality 3}
     ::generate-fn ::add-weapon-price}
    {::index #{20}
     ::generate-fn ::generate-exotic-weapon}]
   ::starting-weapon
   [{::index #{1 2 3 4 5}
     ::description {::name "Dagger"
                    ::damage "1d6 Pierce"
                    ::slot 1
                    ::hand 1
                    ::quality 3}}
    {::index #{6 7}
     ::description {::name "Cudgel"
                    ::damage "1d6 Bludgeon"
                    ::slot 1
                    ::hand 1
                    ::quality 3}}
    {::index #{8}
     ::description {::name "Sickle"
                    ::damage "1d6 Slash"
                    ::slot 1
                    ::hand 1
                    ::quality 3}}
    {::index #{9 10}
     ::description {::name "Mace"
                    ::damage "1d8 Bludgeon"
                    ::slot 2
                    ::hand 1
                    ::quality 3}}
    {::index #{11 12 13 14}
     ::description {::name "Spear"
                    ::damage "1d8 Pierce"
                    ::slot 2
                    ::hand 1
                    ::quality 3}}
    {::index #{15}
     ::description {::name "Hand Ax/Sword"
                    ::damage "1d8 Slash"
                    ::slot 2
                    ::hand 1
                    ::quality 3}}
    {::index #{16}
     ::description {::name "Eku"
                    ::damage "1d8 Slash or Bludgeon"
                    ::slot 3
                    ::hand 2
                    ::quality 3}}
    {::index #{17}
     ::description {::name "Hammer"
                    ::damage "1d10 Bludgeon"
                    ::slot 3
                    ::hand 2
                    ::quality 3}}
    {::index #{18 19}
     ::description {::name "Harpoon"
                    ::damage "1d10 Pierce"
                    ::slot 3
                    ::hand 2
                    ::quality 3}}
    {::index #{20}
     ::description {::name "Ax/Longsword"
                    ::damage "1d10 Slash"
                    ::slot 2
                    ::hand 2
                    ::quality 3}}]
   ::exotic-weapon-trait
   [
    {
     ::index #{1}
     ::description {
                    ::prefix "Scavenged"
                    ::damage-mod "[-]"
                    ::quality -2
                    }
     }
    {
     ::index #{2}
     ::description {
                    ::prefix "Coral-crafted"
                    ::damage-mod "[+]"
                    ::quality +1
                    }
     }
    {
     ::index #{3}
     ::description {
                    ::prefix "Kelp-sling"
                    ::slot -1
                    ::quality +1
                    }
     }
    {
     ::index #{4}
     ::description {
                    ::prefix "Charged"
                    ::damage-mod "[+] Electric"
                    ::quality +1
                    ::special "Glows faintly like a candle"
                    }
     }
    {
     ::index #{5}
     ::description {
                    ::prefix "Vent-forged"
                    ::damage-mod "[+] Heat"
                    ::quality 1
                    }
     }
    {
     ::index #{6}
     ::description {
                    ::prefix "Pressure-forged"
                    ::damage-mod "[+] Cold"
                    ::quality 1
                    }
     }]
   ::ranged-weapon
   [
    {
     ::index #{1 2 3 4 5 6 7 8 9 10 11 12}
     ::description {
                    ::name "Sling"
                    ::ammo "N/A"
                    ::damage "1d4"
                    ::slot 1
                    ::hand 1
                    ::quality 2
                    }
     ::generate-fn ::add-weapon-price
     }
    {
     ::index #{13 14 15 16 17 18}
     ::description {
                    ::name "Bow"
                    ::ammo "Arrows"
                    ::damage "1d6"
                    ::slot 2
                    ::hand 2
                    ::quality 3
                    }
     ::generate-fn ::add-weapon-price
     }
    {
     ::index #{19 20}
     ::description {
                    ::name "Crossbow"
                    ::ammo "Bolts"
                    ::damage "1d8"
                    ::slot 3
                    ::hand 2
                    ::quality 3
                    }
     ::generate-fn ::add-weapon-price
     }]
   ::armor
   [{::index #{1 2 3}
     ::description {::name "No Armor"
                    ::defense 11}}
    {::index #{4 5 6 7 8 9 10 11 12 13 14}
     ::description {::name "Canvas Tunic"
                    ::defense 12
                    ::slot 1
                    ::quality 3}}
    {::index #{15 16 17 18 19}
     ::description {::name "Seal Leather"
                    ::defense 13
                    ::slot 2
                    ::quality 4}}
    {::index #{20}
     ::description {::name "Iron-Scale Mail"
                    ::defense 14
                    ::slot 3
                    ::quality 5}}]
   ::helmets-and-shields
   [{::index #{1 2 3 4 5 6 7 8 9 10 11 12 13}
     ::description {::name "None or Hat"
                    ::defense 11}}
    {::index #{14 15 16}
     ::description {::name "Helmet"
                    ::defense "+1"
                    ::slot 1
                    ::quality 1}}
    {::index #{17 18 19}
     ::description {::name "Shield"
                    ::defense "+1"
                    ::slot 1
                    ::quality 1
                    ::hand 1}}
    {::index #{20}
     ::description {::name "Helmet and Shield"
                    ::defense "+1/+1"
                    ::slot "1/1"
                    ::quality "1/1"
                    ::hand "0/1"}}]
   ::open-water-pelagic
   [{::index #{1}
     ::description "3 Chum float through"}
    {::index #{2}
     ::description (str "A sack of Loot floats past. It contains:\n"
                        "- {{loot}}\n- {{loot}}\n- {{loot}}")
     ::generate-fn ::fill-str-templates}
    {::index #{3 4}
     ::description "Someone sent the dead's Sand down. Gain 500 sand"}
    {::index #{5}
     ::description (str "A wooden figurine. This figurine functions as "
                        "a spellpearl with {{spell}}")
     ::generate-fn ::fill-str-templates}
    {::index #{6}
     ::description "An Eelaconda enters."}]
   ::sinking-ship
   [{::index #{1}
     ::description "A chest of loot! It contains\n- {{loot}}\n- {{loot}}"
     ::generate-fn ::fill-str-templates}
    {::index #{2}
     ::description "A dead sailor is found. If searched:\n{{loot}}"
     ::generate-fn ::fill-str-templates}
    {::index #{3}
     ::description (str "A hermit has set up shop:\n{{gear1-shop}}\n"
                        "{{gear1-shop}}\n{{gear2-shop}}\n"
                        "{{gear2-shop}}\n{{dungear-shop}}\n"
                        "{{dungear-shop}}")
     ::generate-fn ::fill-str-templates}
    {::index #{4}
     ::description (str "There is a noticeable lack of metal on-board. "
                        "Hiding in the hold, chewing on bolts is a Steel "
                        "Beak")}
    {::index #{5}
     ::description (str "The Heirs find 2 Dolpod Cultists worshipping at "
                        "a desecrated temple to the Deep Queen")}
    {::index #{6}
     ::description "The ship is covered in Plague Urchins"}]
   ::general-gear1 (list->generate-map general-gear1 ::fill-name-templates)
   ::general-gear2 (list->generate-map general-gear2 ::fill-name-templates)
   ::a-mana-me (list->generate-map a-mana-me)
   ::spell (list->generate-map spells)
   ::slug-color (list->generate-map slug-colors)
   ::slug-effect (list->generate-map slug-effects)
   ::coral (list->generate-map coral)
   ::physique (list->generate-map physique)
   ::face (list->generate-map face)
   ::hair (list->generate-map hair)
   ::height (list->generate-map height)
   ::speech (list->generate-map speech)
   ::clothing (list->generate-map clothing)
   ::passion (list->generate-map passions)
   ::treasure (list->generate-map treasure)
   })

(defn get-table-names
  ([] (get-table-names tables []))
  ([tables current-path]
   (if (map? tables)
     (reduce
      (fn [c k]
        (concat c (get-table-names (get tables k) (conj current-path k))))
      []
      (keys tables))
     [current-path])))

(defn generate-slug-map []
  (zipmap slug-colors  (shuffle slug-effects)))

(def floor->depth
  {::pelagic 1
   ::the-reef 2
   ::the-kelp-forest 3
   ::the-twilight 4
   ::the-trench 5})

(def depth->floor
  (->> (into [] floor->depth)
       (map reverse)
       flatten
       (apply hash-map)))

(def floor->str
  {::pelagic "Pelagic"
   ::the-reef "The Reef"
   ::the-kelp-forest "The Kelp Forest"
   ::the-twilight "The Twilight"
   ::the-trench "The Trench"})

(def stats [::str ::dex ::con ::int ::wis ::cha])
(def dirs #{:north :east :south :west})
(def dir-map {:north [0 -1] :east [1 0] :south [0 1] :west [-1 0]})
(def opposite-dir {:north :south :east :west :south :north :west :east})
(def floors #{::pelagic ::the-reef ::the-kelp-forest ::the-twilight ::the-trench})

;; Generated Room keys
(s/def ::roll (s/and int? #(> % 0)))
(s/def ::pos-int (s/and int? #(> % 0)))
(s/def ::index (s/coll-of ::pos-int))
(s/def ::danger (s/and int? #(<= -3 0 3)))
(s/def ::name string?)
(s/def ::notes string?)
(s/def ::dir dirs)
(s/def ::from-dir dirs)
(s/def ::description string?)
(s/def ::hallways (s/coll-of ::dir))
(s/def ::secret-hallways (s/coll-of ::dir))
(s/def ::room-index (s/and int? #(>= % 0)))
(s/def ::generated-room (s/keys :req [::name ::hallways ::danger
                                      ::room-index]
                                :opt [::secret-hallways ::situation ::id
                                      ::notes ::index ::adv ::from-dir
                                      ::roll ::description]))

;; Slugs
(s/def ::slug-color (set slug-colors))
(s/def ::slug-effect (set slug-effects))

;; New Character
(s/def ::stat (set stats))
(s/def ::score (s/and int? #(>= 6 % 1)))
(s/def ::scores (s/tuple ::score ::score ::score))
(s/def ::stats (s/map-of ::stat ::scores))
(s/def ::speech (set speech))
(s/def ::hair (set hair))
(s/def ::height (set height))
(s/def ::face (set face))
(s/def ::clothing (set clothing))
(s/def ::physique (set physique))
(s/def ::passion (set passions))
(s/def ::general-gear-1 string?)
(s/def ::general-gear-2 string?)
(s/def ::armor string?)
(s/def ::weapon string?)
(s/def ::dungeoneering-gear string?)

;; Floor state
(s/def ::coord (s/tuple int? int?))
(s/def ::map (s/map-of ::coord ::generated-room))
(s/def ::floor floors)
(s/def ::exit-index (s/and int? #(<= 1 10)))
(s/def ::floor-state (s/keys :req [::map ::floor ::exit-index]))

;; History
(s/def ::time #(time/date? %))
(s/def ::history-event (s/keys :req [::time ::description]
                               :opt [::room-index ::floor]))
(s/def ::history (s/coll-of ::history-event))

;; DB
(s/def ::floors (s/map-of ::floor ::floor-state))
(s/def ::slugs (s/map-of ::slug-color ::slug-effect))
(s/def ::room-adv (s/and int? #(<= -3 % 3)))
(s/def ::current-floor floors)
(s/def ::active-page #{:home :new-character :not-found})
(s/def ::current-room (s/keys :req [::floor ::coord]))
(s/def ::new-character (s/keys :req [::speech ::hair ::height ::face
                                     ::clothing ::physique ::passion
                                     ::general-gear-1 ::general-gear-2
                                     ::armor ::weapon ::dungeoneering-gear
                                     ::stats]))
(s/def ::db (s/keys :req [::current-room ::active-page ::room-adv
                          ::current-floor ::slugs ::floors ::history]
                    :opt [::new-character]))

(defn abs [n] (max n (- n)))

(defn adv->str
  "Convert an advantage/disadvantage number to display string e.g. [+]"
  [none-text adv]
  (if (= 0 adv)
    none-text
    (let [c (if (< 0 adv) \+ \-)]
      (str \[ (apply str (repeat (abs adv) c)) \]))))

(defn str->int [s]
  (js/parseInt s))

(defn roll
  ([num faces] (roll num faces 0))
  ([num faces adv]
   (let [f (if (< adv 0) < >)]
     (->> (repeatedly (+ num (abs adv)) #(inc (rand-int faces)))
          (sort f)
          (take num)
          (apply +)))))

(defn match->roll
  [[_ num faces _ adv]]
  (roll (str->int num) (str->int faces)
        (* (case (first adv) \+ 1 \- -1 1) (count adv))))

;; {{1d4[-]:1:a Chum;2:3 Chum;3:3 Chum and a Chum Guard;
;; 4:4 Chum, 2 Chum Guards and a Chum Priest with {{spell}};
(defn fill-die-roll-template
  "Replace all instances of {{XdY}} in the string with a roll of the dice
   Note the brackets, dice outside of brackets will not be modified"
  [s]
  (str/replace s #"\{\{(\d+)d(\d+)(\[(\++|-+)\])?\}\}" match->roll))

(def choice-pattern #"(\d+)(-(\d+))?:((?:.|\n)*)")
(def roll-choice-pattern #"\{\{(\d+)d(\d+)(\[(\++|-+)\])?:((?:.|\n)*)\}\}")

(defn choice->map [s]
  (let [[_ start _ end choice] (re-find choice-pattern s)
        nums (if end
               (range (str->int start) (inc (str->int end)))
               [(str->int start)])]
    (->> (interleave nums (repeat choice))
         (partition 2)
         (map vec)
         (into {}))))

(defn choices->map [s]
  (->> (str/split s #";")
       (map choice->map)
       (reduce merge)))

(defn match->choice
  [[_ num faces _ adv choice-text]]
  (let [r (roll (str->int num) (str->int faces)
                (* (case (first adv) \+ 1 \- -1 1) (count adv)))
        choice-map (choices->map choice-text)]
    (str/replace (get choice-map r) #"\{\{n\}\}" (str r))))

(defn fill-die-roll-choice-template
  [fs s]
  (str/replace s roll-choice-pattern match->choice))

(defn max-index [templates]
  (->> (map ::index templates)
       (reduce into #{})
       (apply max)))

(defn north?
  "Returns a set, contains :north only if north is a valid direction"
  [[x y]]
  (cond (= 0 y) #{} :else #{:north}))

(defn valid-dirs
  "Returns a set of all valid directions from the given coord"
  [[x y]]
  (if (= 0 y) (disj dirs :north) dirs))

(defn shift-coord
  "Transform the given coordinate to the neighboring coordinate in the given
   direction"
  [coord dir]
  (mapv + coord (dir dir-map)))

(def rotate-cw
  {:north :east
   :east :south
   :south :west
   :west :north})

(def rotate-ccw
  {:north :west
   :east :north
   :south :east
   :west :south})

(defn rotate-room [room dir]
  (let [rotate-map (case dir ::cw rotate-cw ::ccw rotate-ccw)]
    (update room ::hallways #(mapv (partial get rotate-map) %))))

(defn get-neighbors
  "Given floor state and a coord, return a map of direction to neighboring
   rooms of the given coord"
  [floor-map coord]
  (->> (map (juxt identity (partial shift-coord coord)) dirs)
       (map (juxt first (comp (partial get floor-map) second)))
       (filter (comp some? second))))

(defn adjacent-coords
  [coord {h ::hallways sh ::secret-hallways}]
  (zipmap (concat h sh) (map (partial shift-coord coord) (concat h sh))))

(defn get-dirs-with-hallway
  "Get every direction in which an explored room exists with a hallway
   pointing to the given coord"
  [floor-map coord]
  (->> (get-neighbors floor-map coord)
       (map #(update % 1 ::hallways))
       (filter #(some (partial = (get opposite-dir (first %))) (second %)))
       (map first)
       (into #{})))

(defn get-map-bounds
  [floor-map]
  (let [coords (keys floor-map)]
    {:min-x (apply min (map first coords))
     :max-x (apply max (map first coords))
     :min-y 0
     :max-y (apply max (map second coords))}))

(defn get-empty-dirs
  "Given a coordinate and floor map, return a set of all directions from
   that coordinate that do not contain a room"
  [coord floor-map]
  (->> (map (juxt identity (partial shift-coord coord)) dirs)
       (filter #(<= 0 (second (second %))))
       (filter #(not (contains? floor-map (second %))))
       (map first)
       (into #{})))

(defn make-dir-sort-fn
  "Creates a function for sorting directions by preference"
  [coord from-dir floor-map]
  (let [adjacent-dirs (get-dirs-with-hallway floor-map coord)
        adjacent-empty-dirs (get-empty-dirs coord floor-map)]
    (fn [item]
      (cond
        (= item from-dir) 0
        (adjacent-dirs item) 1
        (adjacent-empty-dirs item) 2
        :else 3))))

(defn gen-hallways
  "Given a room template, coordinate of the room, and direction the room
   is generated from, return the room with generated hallways
   (min (+ h sh) num-valid-directions) of the hallways from this room.
   Note: num-valid-directions may not be 4, as north is excluded at y = 0"
  [{h ::num-hallways sh ::num-secret-hallways :or {sh 0} :as room} floor-state coord from-dir]
  (let [sort-fn (make-dir-sort-fn coord from-dir (::map floor-state))] 
    (merge (dissoc room ::num-hallways ::num-secret-hallways)
           (as-> (valid-dirs coord) c
             (shuffle c)
             (sort-by sort-fn c)
             (take (+ h sh) c)
             (split-at h c)
             (filter seq c)
             (map vec c)
             (zipmap [::hallways ::secret-hallways] c)))))

(defn weapon->str
  [{weapon ::description}]
  (str (::name  weapon) "\n"
       (if (::ammo weapon) (str "Ranged, Ammo: " (::ammo weapon) "\n"))
       "Damage: " (::damage weapon) "\n"
       "Slot: " (::slot weapon) "\n"
       "Hand: " (::hand weapon) "\n"
       "Quality: " (::quality weapon) "\n"
       (if (::special weapon) (str "Special: " (::special weapon) "\n"))))

(defn armor->str
  ([armor] (armor->str armor false))
  ([{armor ::description} starting?]
   (if (and (not starting?) (= (::name armor) "No Armor"))
     "Rusted, tattered, useless armor"
     (str (::name armor) "\n"
          "Defense: " (::defense armor) "\n"
          "Slot: " (::slot armor) "\n"
          "Quality: " (::quality armor)))))

(defn generate-slug []
  (first (shuffle slug-colors)))

(defn fill-slug-template
  "Replace every instance of {{slug}} with a random slug"
  [s]
  (str/replace s #"\{\{slug\}\}" #(str/lower-case (generate-slug))))

(defn add-weapon-price
  [_ template]
  (assoc template ::price (* 100 (::roll template))))

(defn generate-spell []
  (first (shuffle spells)))

(defn fill-spell-template
  [s]
  (str/replace s #"\{\{spell\}\}" #(generate-spell)))

(defn rand-template
  "Generate a random object template at the given path in the table map"
  ([object-path] (rand-template object-path 0))
  ([object-path adv]
   (let [templates (get-in tables object-path)
         n (roll 1 (max-index templates) adv)]
     (assoc (first (drop-while #(not (contains? (get % ::index) n)) templates))
            ::roll n))))

(declare fill-templates)

(defn generate
  "Pick a random template from the table map at the given path, and fill in
   any random details"
  ([floor-state object-path] (generate floor-state object-path 0))
  ([floor-state object-path adv]
   (let [template (rand-template object-path adv)]
     (fill-templates floor-state template))))

(defn generate-shop [floor-state]
  (fill-templates floor-state (get-in tables [::situation 2])))

(defn generate-shrine [floor-state]
  (fill-templates floor-state (get-in tables [::situation 7])))

(defn str->adv
  "Given a string of repeated -, + or nil, return the advantage number
   E.g. -- returns -2, + returns 1 and nil returns 0"
  [s]
  (case (first s)
    \+ (count s)
    \- (- (count s))
    0))

(defn fill-melee-weapon [fs s]
  (str/replace s #"\{\{melee-weapon\}\}"
               #(weapon->str (generate fs [::melee-weapon]))))

(defn fill-ranged-weapon [fs s]
  (str/replace s #"\{\{ranged-weapon\}\}"
               #(weapon->str (generate fs [::ranged-weapon]))))

(defn fill-used-ranged-weapon [fs s]
  (let [rw (generate fs [::ranged-weapon])
        used-rw (update-in rw [::description ::quality] dec)]
    (str/replace s #"\{\{used-ranged-weapon\}\}" (weapon->str used-rw))))

(defn fill-rusty-melee-weapon [fs s]
  (let [mw (generate fs [::melee-weapon])
        rusty-mw (assoc-in mw [::description ::quality] 1)]
    (str/replace s #"\{\{rusty-melee-weapon\}\}" (weapon->str rusty-mw))))

(defn fill-dungeoneering-gear [fs s]
  (str/replace s #"\{\{dungeoneering-gear\}\}"
               #(::description (generate fs [::dungeoneering-gear]))))

(defn fill-magic-melee-weapon [fs s]
  (let [mw (generate fs [::melee-weapon])
        magic-mw (update-in mw [::description ::name] #(str % "[+]"))]
    (str/replace s #"\{\{magic-melee-weapon\}\}" (weapon->str magic-mw))))

(defn fill-armor [fs s]
  (str/replace s #"\{\{armor\}\}" (armor->str (generate fs [::armor]))))

(defn fill-used-armor [fs s]
  (let [armor (generate fs [::armor])
        used-armor (update-in armor [::description ::quality] dec)]
    (str/replace s #"\{\{used-armor\}\}"
                 (-> (generate fs [::armor])
                     (update-in [::description ::quality] dec)
                     armor->str))))

(defn fill-var-hallways [fs t]
  (if (string? (::num-hallways t))
    (update t ::num-hallways #(str->int (fill-die-roll-template %)))
    t))

(defn generate-loot
  "Generate a piece of loot for the given floor"
  [floor-state]
  (generate floor-state [::loot]))

(defn fill-loot-template [fs s]
  (str/replace s #"\{\{loot\}\}" #(::description (generate-loot fs))))

(defn generate-shop-item [{floor ::floor :as fs}]
  (let [item (generate-loot fs)
        price (* (::roll item) 50 (floor->depth floor))]
    (assoc item ::price price)))

(defn shop-item->str [{price ::price description ::description}]
  (str price "s - " description))

(defn fill-shop-item [fs s]
  (str/replace s #"\{\{shop-item\}\}"
               #(shop-item->str (generate-shop-item fs))))

(defn fill-shop-inventory [fs s]
  (let [depth (floor->depth (::floor fs))]
    (str/replace
     s #"\{\{shop-inventory\}\}"
     (fn [_]
       (str/join "\n"
                 (for [_ (range depth)]
                   (shop-item->str (generate-shop-item fs))))))))

(defn fill-encounter [{floor ::floor :as fs} s]
  (str/replace s #"\{\{encounter(-+?|\++?)?\}\}"
               (fn [[_ adv-str]]
                 (->> (generate fs [::encounter floor] (str->adv adv-str))
                      ::description))))

(defn fill-twilight-encounter [fs s]
  (str/replace s #"\{\{twilight-encounter\}\}"
               (::description (generate fs [::encounter ::the-twilight]))))

(defn fill-depth-dice [{floor ::floor} s]
  (str/replace s #"\{\{depthd(\d+)\}\}"
               (fn [[_ faces-str]]
                 (str (roll (floor->depth floor) (str->int faces-str))))))

(defn fill-situation-loot [{floor ::floor :as fs} s]
  (str/replace
   s #"\{\{situation-loot\}\}"
   (fn [_]
     (str/join "\n"
               (for [_ (range (floor->depth floor))]
                 (::description (generate fs [::loot])))))))

(defn fill-rogue-wave-contents [fs s]
  (str/replace s #"\{\{rogue-wave-contents\}\}"
               #(::description (generate fs [::rogue-wave-contents]))))

(defn fill-god-template [fs s]
  (str/replace s #"\{\{god\}\}"
               #(::name (generate fs [::god]))))

(defn fill-deep-pool-attribute [fs s]
  (str/replace s #"\{\{deep-pool-attribute\}\}"
               #(::description (generate fs [::deep-pool-attribute]))))

(defn fill-open-water-pelagic [fs s]
  (str/replace s #"\{\{open-water-pelagic\}\}"
               #(::description (generate fs [::open-water-pelagic]))))

(defn fill-trap [{floor ::floor :as fs} s]
  (str/replace s #"\{\{trap\}\}"
               #(::name (generate fs [::trap floor]))))

(defn fill-sinking-ship [fs s]
  (str/replace s #"\{\{sinking-ship\}\}"
               #(::description (generate fs [::sinking-ship]))))

(defn generate-gear-shop-item [fs gear]
  (let [item (generate fs [gear])
        price (* (::roll item) 50)]
    (assoc item ::price price)))

(defn shop-gear->str [{price ::price name ::name}]
  (str price "s - " name))

(defn fill-gear-shop [gear template fs s]
  (str/replace
   s
   template
   #(shop-gear->str (generate-gear-shop-item fs gear))))

(def fill-gear1-shop
  (partial fill-gear-shop ::general-gear1 #"\{\{gear1-shop\}\}"))
(def fill-gear2-shop
  (partial fill-gear-shop ::general-gear2 #"\{\{gear2-shop\}\}"))
(def fill-dungear-shop
  (partial fill-gear-shop ::dungeoneering-gear #"\{\{dungear-shop\}\}"))

(defn fill-str-templates
  "Fill all templates of the form {{command}}"
  ([fs t] (fill-str-templates fs t ::description))
  ([fs t k]
   (-> (update t k (partial fill-die-roll-choice-template fs))
       (update k fill-die-roll-template)
       (update k fill-slug-template)
       (update k fill-spell-template fs)
       (update k (partial fill-loot-template fs))
       (update k (partial fill-melee-weapon fs))
       (update k (partial fill-ranged-weapon fs))
       (update k (partial fill-used-ranged-weapon fs))
       (update k (partial fill-rusty-melee-weapon fs))
       (update k (partial fill-dungeoneering-gear fs))
       (update k (partial fill-magic-melee-weapon fs))
       (update k (partial fill-armor fs))
       (update k (partial fill-used-armor fs))
       (update k (partial fill-shop-item fs))
       (update k (partial fill-depth-dice fs))
       (update k (partial fill-encounter fs))
       (update k (partial fill-twilight-encounter fs))
       (update k (partial fill-situation-loot fs))
       (update k (partial fill-shop-inventory fs))
       (update k (partial fill-god-template fs))
       (update k (partial fill-rogue-wave-contents fs))
       (update k (partial fill-deep-pool-attribute fs))
       (update k (partial fill-open-water-pelagic fs))
       (update k (partial fill-trap fs))
       (update k (partial fill-sinking-ship fs))
       (update k (partial fill-gear1-shop fs))
       (update k (partial fill-gear2-shop fs))
       (update k (partial fill-dungear-shop fs)))))

(defn generate-exotic-weapon
  "generation-fn for an exotic weapon. Generates a new non-exotic weapon,
   an exotic trait, and combines them"
  [fs template]
  (let [weapon-template (->> (repeatedly #(generate fs [::melee-weapon]))
                             (drop-while #(= (::roll %) 20))
                             first)
        {exotic ::description} (generate fs [::exotic-weapon-trait])]
    (->> (assoc template ::description
                (merge-with
                 +
                 (-> (::description weapon-template)
                     (update ::name #(str (::prefix exotic) " " %))
                     (update ::damage #(str/trim (str % " " (::damage-mod exotic)))))
                 (select-keys exotic [::quality ::slot ::special])))
         (add-weapon-price fs))))

(defn fill-templates [fs t]
  (-> (case (::generate-fn t)
        ::fill-str-templates (fill-str-templates fs t)
        ::fill-name-templates (fill-str-templates fs t ::name)
        ::generate-exotic-weapon (generate-exotic-weapon fs t)
        ::add-weapon-price (add-weapon-price fs t)
        t)
      ((partial fill-var-hallways fs))
      (dissoc ::generate-fn)))

(defn get-exit-room-name [floor]
  (if (#{::the-trench} floor)
    "Deep Queen's Throne"
    (str "Exit to " (floor->str
                     (get depth->floor (inc (get floor->depth floor)))))))

(defn generate-exit-room
  [{floor ::floor}]
  (merge (::exit universal-rooms)
         {::name (get-exit-room-name floor)}))

(defn gen-random-room
  [floor-state adv from-dir]
  (if (= (::exit-index floor-state) (count (::map floor-state)))
    (generate-exit-room floor-state)
    (-> (generate floor-state [::room (::floor floor-state)] adv)
        (merge {::from-dir from-dir ::adv adv}))))

(defn add-situation
  "Generate a random situation and add it to the given room if applicable"
  [room floor-state adv]
  (if (#{::exit} (::id room))
    room
    (merge room {::situation (generate floor-state [::situation] adv)})))

(defn generate-room
  ([floor-state coord from-dir] (generate-room floor-state coord from-dir 0))
  ([floor-state coord from-dir adv]
   (assoc-in
    floor-state
    [::map coord]
    (-> (gen-random-room floor-state adv from-dir)
        (gen-hallways floor-state coord from-dir)
        (merge {::room-index (count (::map floor-state))})
        (add-situation floor-state adv)))))

(defn generate-trait [trait]
  (::name (generate nil [trait])))

(def character-traits
  [::physique ::face ::hair ::height ::speech ::clothing ::passion])

(def starting-equipment
  [::weapon ::armor ::helmet-and-shield ::general-gear-1
   ::general-gear-2 ::dungeoneering-gear])

(defn generate-starting-equipment []
  {::weapon (weapon->str (generate nil [::starting-weapon]))
   ::armor (armor->str (generate nil [::armor]) true)
   ::helmet-and-shield (armor->str (generate nil [::helmets-and-shields]))
   ::general-gear-1 (::name (generate nil [::general-gear1]))
   ::general-gear-2 (::name (generate nil [::general-gear2]))
   ::dungeoneering-gear (::description (generate nil [::dungeoneering-gear]))})

(defn roll-stats []
  (repeatedly 3 (partial roll 1 6)))

(defn generate-stat-block []
  (->> (repeatedly 6 roll-stats)
       (map (comp vec sort))
       (zipmap stats)))

(defn generate-new-character []
  (-> (into {} (map (juxt identity generate-trait) character-traits))
      (merge (generate-starting-equipment))
      (assoc ::stats (generate-stat-block))))

(defn init-floor
  "Initiate a floor in the db for the given level e.g. :pelagic"
  [level]
  {::map (case level
           ::the-reef
           {[0 0] (assoc (::the-plunge-reef universal-rooms)
                         ::room-index 0)
            [0 1] (merge (::the-great-barrier universal-rooms)
                         {::situation (generate {::floor level} [::situation])
                          ::room-index 1})}
           {[0 0] (assoc (::the-plunge universal-rooms) ::room-index 0)})
   ::floor level
   ::exit-index (roll 1 10 2)})

(defn init-floors []
  (reduce #(assoc %1 %2 (init-floor %2)) {} floors))

(defn init-db
  ([db] (init-db db true))
  ([db reset-slugs?]
   (merge (-> db
              (assoc ::active-page :home)
              (assoc ::current-floor ::pelagic)
              (assoc ::floors (init-floors))
              (assoc ::history [])
              (assoc ::room-adv 0)
              (assoc ::current-room {::floor ::pelagic
                                     ::coord [0 0]}))
          (if reset-slugs? {::slugs (generate-slug-map)}))))

(def default-db (init-db  {:name "shifting-sands"}))

;; EDN

(def ls-key "app-state")

;; Keys to save to local storage from the app DB
(def state-keys [::current-room ::current-floor ::slugs ::floors ::history])

(def iso8601-formatter (f/formatters :date-time))

(defn- datetime->reader-str [d]
  (str "#DateTime \"" (f/unparse iso8601-formatter d) \"))

(defn- reader-str->datetime [s]
  (f/parse iso8601-formatter s))

(do (extend-protocol IPrintWithWriter
      goog.date.DateTime
      (-pr-writer [d out opts]
        (-write out (datetime->reader-str d)))))

(defn state->local-store
  "Puts app state (map, history, current-room, etc.)  into localStorage"
  [db]
  (.setItem js/localStorage ls-key (str (select-keys db state-keys)))) 

(re-frame/reg-cofx
 :local-store-state
 (fn [cofx _]
   ;; put the localstore state into the coeffect under :local-store-state
   (assoc cofx :local-store-state
          ;; read the state from localstore, and process into a map
          (some->> (.getItem js/localStorage ls-key)
                   (reader/read-string
                    {:readers {'DateTime reader-str->datetime}})))))
