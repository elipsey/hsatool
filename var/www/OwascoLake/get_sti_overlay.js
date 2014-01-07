function getStiOverlay(theSatPercent)
{
	//location of saturation overlay layers
	//0% saturation gets no overlay
	//all layers >30% saturation currently point to the 30% saturation layer
	var overlayLocation = { 0: "",
				10: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco10.kmz",
				20: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco20.kmz",
				30: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				40: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				50: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				60: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				70: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				80: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				90: "http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz",
				100:"http://hsadss.bee.cornell.edu/sti_layers/OwascoLake/owasco30.kmz"
				}
	return overlayLocation[theSatPercent];
}
