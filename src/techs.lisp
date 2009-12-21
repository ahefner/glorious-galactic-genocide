(in-package :g1)

;;;; Underlying protocols for techs

(defun can-colonize? (tech planet-type)
  (and tech (find planet-type (slot-value tech 'colonizable))))


;;;; Tech definitions

(clrhash *name->tech*)

;;; Misc. starting techs

(deftech (0 special-tech reserve-tanks :range-bonus 3)
    "Extends ship range by 3 light-years.")

(defparameter *friendly-planet-types* 
  '(terran oceanic jungle arid desert tundra minimal))

(deftech (0 special-tech colony-base :colonizable *friendly-planet-types*)
    "A colony base can be used to create a new settlement on any uninhabited planet with a breathable atmosphere. The colony ship is dismantled and recycled by the new colony.")


;;;; Engines

(deftech (0 engine ion-drive :engine-speed 1)
    "Allowing acceleration up to half the speed of light, the Ion Drive made interstellar travel practical for the first time.")

(deftech (4 engine pulse-drive :engine-speed 2)
    "Reaching nearly the speed of light, the Pulsed Nuclear Drive offers a substantial speed improvement over the basic Ion Drive, but is only practical on larger ships.")

(deftech (7 engine sublight-drive :engine-speed 2)
    "Sublight Drives are a miniaturized engine technology that can propel a ship to very nearly the speed of light. Comparable with Pulse Drives in speed, the smaller Sublight Drive consumes less space and can be equipped on ships of any size.")

(deftech (9 engine stardrive :engine-speed 3)
    "Revolutionary advances in physics lead to the proposal of the Stardrive, the first FTL engine. Capable of accelerating a starship up to 150% of the speed of light, it enables travel three times faster than the standard Ion Drive.")

(deftech (12 engine hyperdrive :engine-speed 6 :probability 0.20)
    "The Hyperdrive creates a tunnel through shallow hyperspatial dimensions, allowing a ship to emerge at its destination having appeared to travel at three times the speed of light. The energy required to maintain the tunnel increases exponentially with size, making Hyperdrives impractical for larger vessels.")

(deftech (18 engine warp-engine :engine-speed 4)
    "A refinement of the Stardrive principle, Warp Engines allow a ship to travel at up to twice the speed of light.")

(deftech (27 engine tachyon-engine :engine-speed 5)
    "Originally proposed in 2089 by an obscure Human science fiction author named Jamal Huso, his \"tachyon engine\" was dismissed as quackery and forgotten until recently, when a theoretical physicist chanced upon one of his novels and took credit for the invention himself. The tachyon engine can accelerate a ship up to 2.5x the speed of light.")

(deftech (35 engine trans-light-drive :name "Trans-light Drive" :engine-speed 6)
    "The ultimate refinement of the principle behind the Stardrive and Warp Drive, Trans-light Drives can propel a starship at up to three times the speed of light.")

(deftech (46 engine tachyon-gradient-engine :name "Tachyon-Gradient Engine" :engine-speed 8)
    "A high-efficiency refinement of the Tachyon Engine capable of accelerating a starship to four times the speed of light.")

;;;; Armor

(deftech (0 hull titanium-hull :armor-level 0)
    "The standard titanium hull protects a starship from the rigors of interstellar travel.")

(deftech (0 armor titanium-armor :armor-level 2 :hull-modifier 1.5 :linked-to (find-tech 'titanium-hull))
     "A reinforced hull with thicker titanium armor plating protects vital ship systems in case of attack.")

(deftech (5 hull duranium-hull :armor-level 1 :hull-modifier 1.25)
    "A lightweight hull of synthetic duranium provides protection comparable with titanium armor at a fraction of its weight.")

(deftech (5 armor duranium-armor :armor-level 3 :hull-modifier 1.75 :linked-to (find-tech 'duranium-hull))
    "Heavier duranium armor provides robust protection for a ship's vital systems.")

(deftech (9 armor ablative-armor :armor-level 1 :hull-modifier 1.75 :beam-defense 5)
    "Ablative armor provides advanced defense against beam weapons.")

(deftech (12 hull plasteel-hull :armor-level 2 :hull-modifier 1.50)
    "A plasteel hull offers suitable protection for ships in combat, without the weight or cost of heavy armor.")

(deftech (12 armor plasteel-armor :armor-level 4 :hull-modifier 2.00 :linked-to (find-tech 'plasteel-hull))
    "Plasteel armor can protect our heaviest warships in the thick of combat.")

(deftech (15 armor neutronium-armor :armor-level 4 :hull-modifier 3.00 :probability 0.30)
    "Heavy neutronium armor renders a ship's hull nearly impregnable.")

(deftech (16 armor macromolecular-armor :armor-level 2 :hull-modifier 1.75 :probability 0.35)
    "Macromolecular armor has the ability to reform and repair weak spots at a rate of up to 15% per turn, but repeated attacks will eventually wear it away.")

(deftech (21 hull endurium-hull :armor-level 2 :hull-modifier 2.0)
    "Endurium hulls provide an excellent compromise between weight and protection during combat.")

(deftech (21 armor endurium-armor :armor-level 4 :hull-modifier 2.75 :linked-to (find-tech 'endurium-hull))
    "Endurium armor provides nearly the protection of neutronium armor, at a fraction of the cost and weight.")

(deftech (32 hull unobtainium-hull :armor-level 3 :hull-modifier 2.50)
    "Unobtainium hulls provide the greatest protection for starships not carrying heavy armor.")

(deftech (32 armor unobtainium-armor :armor-level 4 :hull-modifier 3.50 :linked-to (find-tech 'unobtainium-hull))
    "Unobtainium armor can protect a starship under unrelenting weapons fire.")

(deftech (40 armor megalloy-armor :armor-level 5 :hull-modifier 4.00 :probability 0.25)
    "Ultra-dense megalloy armor can deflect all but the heaviest weapons fire.")

;;;; Fuel technology

(deftech (0 fuel fission-power-plant :range-bonus 4)
    "The standard fission power plant allows a starship to travel up to 4 light-years from the nearest colony.")

(deftech (2 fuel high-efficiency-power-plant :name "High-efficiency Power Plant" :range-bonus 5)
    "This improved power plant allows a starship to travel up to 5 light-years from the nearest colony.")

(deftech (4 fuel fusion-generators :range-bonus 6)
    "Fusion generators allow a starship to travel up to 6 light-years from the nearest colony.")

(deftech (7 fuel enhanced-fusion-generators :range-bonus 7)
    "Enhanced fusion generators allow a starship to travel up to 7 light-years from the nearest colony.")

(deftech (11 fuel hyperfusion-generator :range-bonus 8)
    "Hyperfusion generators allow a starship to travel up to 8 light-years from the nearest colony.")

(deftech (18 fuel antimatter-generator :range-bonus 10)
    "Antimatter generators allow a starship to travel up to 10 light-years from the nearest colony.")

(deftech (30 fuel enhanced-antimatter-generator :range-bonus 12)
    "Enhanced antimatter generators allow a starship to travel up to 12 light-years from the nearest colony.")

(deftech (40 fuel antimatter-power-core :range-bonus 15)
    "Antimatter power cores allow a starship to travel up to 15 light-years from the nearest colony.")

;;;; Shields

(deftech (2 shield deflector-shield :shield-level 1)
    "Deflector shields absorb one point of damage from each enemy attack.")

(deftech (4 shield enhanced-deflector-shield :shield-level 2)
    "Enhanced deflector shields absorb two points of damage from each enemy attack.")

(deftech (6 shield superior-deflector-shield :shield-level 3)
    "Superior deflector shields absorb three points of damage from each enemy attack.")

(deftech (9 shield class-4-deflector-shield :shield-level 4 :name "Class IV Deflector Shield")
    "Class IV deflector shields absorb four points of damage from each enemy attack.")

(deftech (11 shield anti-projectile-shield :shield-level 3 :name "Anti-Projectile Shield")
    "Anti-projectile shields deflect 50% of light projectile and flak attacks, deflect heavy projectile weapons 15% of the time, and absorb three points of damage from each energy or explosive weapon attack.")

(deftech (14 shield class-5-deflector-shield :shield-level 5 :name "Class V Deflector Shield")
    "Class V deflector shields absorb five points of damage from each enemy attack.")

(deftech (18 shield class-6-deflector-shield :shield-level 6 :name "Class VI Deflector Shield")
    "Class VI deflector shields absorb six points of damage from each enemy attack.")

(deftech (20 shield reflective-shield :shield-level 5 :name "Reflective Shield")
    "Reflective shields absorb five points of damage from each enemy attach and reflect up to 50% of beam weapon fire back at the enemy.")

(deftech (24 shield class-7-deflector-shield :shield-level 7 :name "Class VII Deflector Shield")
    "Class VII deflector shields absorb seven points of damage from each enemy attack.")

(deftech (29 shield class-8-deflector-shield :shield-level 8 :name "Class VIII Deflector Shield")
    "Class VII deflector shields absorb eight points of damage from each enemy attack.")

(deftech (36 shield class-9-deflector-shield :shield-level 9 :name "Class IX Deflector Shield")
    "Class IX deflector shields absorb nine points of damage from each enemy attack.")

(deftech (41 shield class-10-deflector-shield :shield-level 10 :name "Class X Deflector Shield")
    "Class X deflector shields absorb ten points of damage from each enemy attack.")

(deftech (48 shield class-11-deflector-shield :shield-level 11 :name "Class XI Deflector Shield")
    "Class XI deflector shields absorb eleven points of damage from each enemy attack.")

;;;; Non-missile Weapons

(deftech (0 beam laser-beam :damage (range 1 5))
    "Laser beams have been standard defensive weapons since the earliest days of space warfare.")

(deftech (0 beam heavy-laser-beam :damage (range 2 9) :linked-to (find-tech 'laser-beam) :sizemod 3.0 :costmod 3.0 :range 2)
    "Heavy lasers provide the punch necessary to penetrate heavy armor.")

(deftech (1 beam pulse-laser :damage (range 2 6) :costmod 1.2)
    "Pulse lasers are an incremental improvement on basic laser beam technology, able to inflict slightly more damage.")

(deftech (3 projectile mass-driver :damage (range 2 8) :sizemod 1.4)
    "Mass drivers launch high-velocity slugs capable of tearing through heavy armor.")

(deftech (5 particle-weapon pulse-cannon :damage (range 3 9) :sizemod 1.2 :costmod 1.4 :range 1.5)
    "Pulse cannons fire a tightly focussed burst of high energy nucleons at the target.")

(deftech (6 beam positron-beam :damage (range 1 10))
    "Positron beams are the logical successor to laser-based weaponry on smaller craft.")

(deftech (6 beam heavy-positron-beam :linked-to (find-tech 'positron-beam) :damage (range 3 15) :sizemod 3.0 :costmod 3.0 :range 2)
    "Heavy positron beams provide larger ships with the firepower necessary against shielded, armored adversaries.")

(deftech (7 projectile flak-cannon :damage (range 2 9) :sizemod 1.5 :range 2)
    "Designed to protect against fighter attacks, the flak cannon fires exploding bursts of metal fragments to strike all craft in an area. The effectiveness of flak cannons increases with the number of targets.")

(deftech (8 particle-weapon neutron-blaster :damage (range 3 15) :sizemod 1.5 :costmod 1.2 :range 1.5)
    "The neutron blaster is a particle weapon effective against both small and large targets.")

(deftech (9 beam neutron-streamer :damage (range 1 12) :costmod 2.0 :shield-scaling 0.5)
    "Neutron streamers are an advanced beam weapon designed to counter enemy shields, halving their effectiveness.")

(deftech (12 particle-weapon particle-wave-generator :damage (range 1 12) :costmod 2.5)
    "Best suited against large numbers of small ships, a particle wavefront expands to strike many ships simultaneously. Its damage potential increases with the number of targets.")

(deftech (14 particle-weapon particle-cannon :damage (range 5 15) :sizemod 0.9 :costmod 1.3)
    "Lightweight particle cannons pack a powerful punch while remaining suitable for use on ships of all sizes.")

(deftech (14 particle-weapon heavy-particle-cannon :linked-to (find-tech 'particle-cannon) :damage (range 10 25) :sizemod 3.0 :costmod 3.0 :range 2.5)
    "Heavy particle cannons have the firepower to neutralize virtually any shields or armor.")

(deftech (18 particle-weapon plasma-cannon :damage (range 10 20) :sizemod 2.0 :costmod 2.2)
    "Plasma cannons bring devastating firepower to bare on the enemy.")

(deftech (21 beam antigraviton-beam :name "Anti-graviton Beam" :damage (range 10 20) :costmod 1.3)
    "Anti-graviton beams disrupt the structure of matter, tearing enemy craft apart.")

(deftech (22 projectile starburst-defense-battery :damage (range 10 25) :sizemod 3.0 :costmod 5.0 :range 2.0)
    "Sophisticated anti-fighter defense, the Starburst Defense Battery fires fragmenting antimatter shells whose effectiveness increases with the number of targets.")

(deftech (25 beam ripper-beam :damage (range 10 20) :costmod 2.5 :shield-scaling 0.5)
    "A space-distorting weapon designed to bypass enemy shields, ripper beams render opposing shields only 50% effective.")

(deftech (27 particle-weapon pyroquark-blaster :damage (range 15 35) :costmod 4.0 :sizemod 2.5 :range 2)
    "A fearsome heavy particle weapon weapon developed for the largest warships.")

(deftech (29 projectile neutronium-rail-gun :damage (range 10 60) :costmod 9.0 :sizemod 5.0 :range 4)
    "The railgun fires a heavy slug of ultra-dense neutronium, striking with devastating damage at up to four times standard firing range.")

(deftech (31 beam phasor :damage (range 15 25) :sizemod 1.1 :costmod 1.1)
    "A powerful and versatile beam weapon, the phasor combines high firepower with modest space and power requirements.")

(deftech (36 particle-weapon antimatter-cannon :damage (range 10 50) :sizemod 2.0 :costmod 4.0 :range 1.5)
    "Antimatter cannons are brutal heavy weapons designed for the most fearsome warships in the galaxy.")

(deftech (42 beam alpha-beam :damage (range 15 40) :costmod 1.5)
    "The ultimate beam weapon.")

;;;; Missiles and Torpedos

(deftech (0 missile megaton-missile :damage (range 2 7) :projectile-speed 2 :weapon-targetting-bonus 0)
    "Multi-megaton nuclear warheads, adapted from pre-spacefaring nuclear arsenals.")

(deftech (4 missile zarite-missile :damage (range 3 9) :projectile-speed 2.5 :weapon-targetting-bonus 1)
    "An improved nuclear missile technology with improved accuracy and capable of inflicting greater damage.")

(deftech (8 missile hyperzarite-missile :damage (range 4 10) :projectile-speed 3 :weapon-targetting-bonus 2 :probability 0.30)
    "Hyperzarite Missiles travel faster, are more likely to hit, and inflict slightly more damage than regular Zarite missiles.")

(deftech (10 torpedo emp-torpedo :name "EMP Torpedo" :probability 0.20 :damage (range 1 20) :projectile-speed 1.5 :weapon-targetting-bonus 6 :sizemod 2.5 :costmod 4.0)
    "Using an electromagnetic pulse to damage electronics and potentially disable groups of ships for a turn, EMP torpedos can only be fired every other turn.")

(deftech (11 missile neptunium-missile :damage (range 6 14) :projectile-speed 3 :weapon-targetting-bonus 2)
    "Neptunium missiles combine the superior speed and targetting of Hyperzarite missiles with a more potent explosive charge.")

(deftech (13 missile hyperneptunium-missile :damage (range 7 15) :projectile-speed 3.5 :weapon-targetting-bonus 3 :probability 0.30)
    "Hyperneptunium missiles improve on Neptunium missiles with greater speed, accuracy, and slightly improved damage potential.")

(deftech (15 torpedo photon-torpedo :damage (range 12 30) :projectile-speed 1.5 :weapon-targetting-bonus 4)
    "Photon torpedos are far more destructive and accurate than missiles, but travel more slowly and can only be fired every other turn.")

(deftech (18 missile protoquark-missile :damage (range 10 20) :projectile-speed 3.5 :weapon-targetting-bonus 4)
    "Protoquark missiles use an advanced matter-disrupting warhead and travel at a speed of 3.5, with a targetting bonus of +4.")

(deftech (26 missile advanced-protoquark-missile :damage (range 15 25) :projectile-speed 3.5 :weapon-targetting-bonus 5)
    "Advanced protoquark missiles offer improved accuracy and do considerably greater damage than standard Protoquark missiles.")

(deftech (30 torpedo antimatter-torpedo :damage (range 30 60) :projectile-speed 1.5 :weapon-targetting-bonus 6 :sizemod 3.0 :costmod 2.5)
    "Antimatter torpedos unleash unprecedented destructive power, but travel more slowly than missiles and can only be fired every other turn.")

(deftech (39 missile ultron-missile :damage (range 20 30) :projectile-speed 4 :weapon-targetting-bonus 6)
    "Ultron missiles are the pinnacle of missile technology.")



