aboutText <- mainPanel(
    HTML("<h2>Community Nigrogen Footprint Calculator </h2>
The Community Nitrogen Footprint Calculator is an online tool intended to help show the nitrogen footprint for a locality. The amount of nitrogen from each census block group is calculated and then shown in graphs, maps, and a table. This tool also makes predictions to help show the effect of possible policies that could reduce this footprint.
<br />
<br />
<h3>
Data Inputs:
</h3>
<h4>Customer Expenditure Report Data (CEX):</h4> This table should contain all of the columns it originally did when downloaded. If you wish to omit some census block groups, you may delete those rows. However, there are options to omit them from graphs so this is not necessary.
<h4>Data by Block Group:</h4>Download the file below, insert the data, and reupload to the \"Block Group Entry\" file input. This is where the population per census block group and the number of businesses per census block group are entered.This information could likely be found at American Fact Finder.
<br /> "),
    downloadButton("downloadBlockGroupInput", "Download Block Group Data Input Template"),
    HTML("
<h4>Region:</h4> This input refers to the eGRID region for the locality. More information can be found here: <a href=https://www.epa.gov/energy/power-profiler>https://www.epa.gov/energy/power-profiler </a>
<br />
<h4>Number of people/businesses per Census Block Group:</h4>  This population data can be found at American FactFinder, using the most recent 5 year American Communities Survey (ACS).
<br />
<h4>Percentage of households on SNAP:</h4> This information also should be entered into the Block Group Data Input file. 
<br />
<h4>Number of Cats/Dogs:</h4> The average number of cats and dogs per person for all of the block groups combined. The default values of 0.267 cats per person and 0.242 dogs per person are the national average.
<br />
<h4>Gallons of Wastewater:</h4> This information should be available at the local municipal wastewater treatment plant.
<br />
<h4>Transportation Miles:</h4> The state's Department of Transportation should have information on the total number of millions of miles different vehicles travelled in the locality. Below is a chart relating the 13 FHWA (Federal Highway Administration) vehicle classifications to the 5 categories used in the tool (motorcycles, passenger cars, busses, light trucks, and medium-heavy trucks. 

<table>
<tr>
<th>FHWA Class Group </th><th>	FHWA Class Definition	</th><th> FHWA Class Includes	</th><th> Number of Axles	</th><th> Tool Designation </th>
</tr><tr>
                  <th>1</th><th>	Motorcycles</th><th>	Motorcycles</th><th>	2	</th><th>Motorcycles</th>
</tr><tr>
                  <th> 2</th>
                  <th>	Passenger Cars</th>
                  <th>	All cars
                  Cars with one-axle trailer
                  Cars with two-axle trailers</th>
                  <th>	2, 3, or 4</th>
                  <th>	Passenger Cars</th>
</tr><tr>
                  <th> 3	</th>
                  <th>Other Two-Axle Four-Tire Single Unit Vehicles	</th>
                  <th> Pick-ups and vans
                  Pick-ups and vans with one- and two- axle trailers </th>
                  <th>	2, 3, or 4</th>
                  <th>	Light Trucks </th>
</tr><tr>
                  <th>4</th>
                  <th>	Buses</th>
                  <th>	Two- and three-axle buses</th>
                  <th>	2 or 3</th>
                  <th>	Buses</th>
</tr><tr>
                  <th>5</th>
                  <th>	Two-Axle, Six-Tire, Single-Unit Trucks</th>
                  <th>	Two-axle trucks</th><th>	2</th>
                  <th>	Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>6</th>
                  <th>	Three-Axle Single-Unit Trucks</th>
                  <th>	Three-axle trucks
                  Three-Axle tractors without trailers</th>
                  <th>	3	</th>
                  <th>Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>7</th>
                  <th>	Four or More Axle-Single-Unit Trucks</th>
                  <th>	Four-, five-, six-, or seven-axle single-unit trucks</th>
                  <th>	4 or more</th>
                  <th>	Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>8</th>
                  <th>	Four or Fewer Axle Single-Trailer Trucks</th>
                  <th>	Two-axle trucks pulling one- and two-axle trailers
                  Two-axle tractors pulling one- and tow-axle trailers
                  Three-axle tractors pulling one-axle trailers	</th>
                  <th>3 or 4	</th>
                  <th>Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>9</th>
                  <th>	Five-Axle Single-Trailer Trucks</th>
                  <th>	Two-axle tractors pulling three-axle trailers
                  Three-axle tractors pulling two-axle trailers
                  Three-axle trucks pulling tow-axle trailers	</th>
                  <th>5</th>
                  <th>	Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>10</th>
                  <th>	Six or More Axle Single-Trailer Trucks</th>
                  <th>	Multiple configurations</th>
                  <th>	6 or more	</th>
                  <th>Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>11</th>
                  <th>	Five or Fewer Axle Multi-Trailer Trucks</th>
                  <th>	Multiple configurations	</th>
                  <th>4 or 5</th>
                  <th>	Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>12	</th>
                  <th>Six-Axle Multi-Trailer Trucks</th>
                  <th>	Multiple configurations</th>
                  <th>	6	</th>
                  <th>Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>13</th>
                  <th>	Seven or More Axle Multi-Trailer Trucks	</th>
                  <th>Multiple configurations	</th>
                  <th>7 or more</th>
                  <th>	Medium-Heavy Duty Trucks</th>
</tr><tr>
                  <th>14</th>
                  <th>	Unused	</th>
                  <th>----</th>
                  <th>	----</th>
                  <th>	Not classified</th>
</tr><tr>
                  <th>15</th>
                  <th>	Unclassified Vehicle</th>
                  <th>	Multiple configurations	</th>
                  <th>2 or more	</th>
                  <th>Not classified</th> 
</tr> </table>
                  Adapted from Chapter 2 of FHWA Publication Number:  FHWA-HRT-13-091
<br />
<h4>Electricity/ Natural Gas:</h4> This information should be available at the local utility.

")
                       )
