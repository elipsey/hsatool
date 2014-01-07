function getStiOverlay(theSatPercent)
{
	//location of saturation overlay layers
	//0% saturation gets no overlay
	//all layers >30% saturation currently point to the 30% saturation layer
	var overlayLocation = { 0: "",
				10: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_10pct.kmz",
				20: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_20pct.kmz",
				30: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				40: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				50: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				60: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				70: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				80: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				90: "http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz",
				100:"http://hsadss.bee.cornell.edu/sti_layers/SalmonCreek/SalmonCreek_30pct.kmz"
				}
	return overlayLocation[theSatPercent];
}
