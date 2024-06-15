library(dplyr)

# データをテキスト形式で定義
data_text <- "
AL|01|01779775|Alabama|0|10000
AK|02|01785533|Alaska|1|2013
AZ|04|01779777|Arizona|0|10000
AR|05|00068085|Arkansas|0|10000
CA|06|01779778|California|1|2015
CO|08|01779779|Colorado|1|2011
CT|09|01779780|Connecticut|0|10000
DE|10|01779781|Delaware|0|10000
DC|11|01702382|District of Columbia|1|2013
FL|12|00294478|Florida|0|10000
GA|13|01705317|Georgia|0|10000
HI|15|01779782|Hawaii|0|10000
ID|16|01779783|Idaho|0|10000
IL|17|01779784|Illinois|0|10000
IN|18|00448508|Indiana|0|10000
IA|19|01779785|Iowa|0|10000
KS|20|00481813|Kansas|0|10000
KY|21|01779786|Kentucky|0|10000
LA|22|01629543|Louisiana|0|10000
ME|23|01779787|Maine|1|2015
MD|24|01714934|Maryland|0|10000
MA|25|00606926|Massachusetts|1|2015
MI|26|01779789|Michigan|1|2017
MN|27|00662849|Minnesota|0|10000
MS|28|01779790|Mississippi|0|10000
MO|29|01779791|Missouri|0|10000
MT|30|00767982|Montana|0|10000
NE|31|01779792|Nebraska|0|10000
NV|32|01779793|Nevada|1|2015
NH|33|01779794|New Hampshire|0|10000
NJ|34|01779795|New Jersey|0|10000
NM|35|00897535|New Mexico|0|10000
NY|36|01779796|New York|0|10000
NC|37|01027616|North Carolina|0|10000
ND|38|01779797|North Dakota|0|10000
OH|39|01085497|Ohio|0|10000
OK|40|01102857|Oklahoma|0|10000
OR|41|01155107|Oregon|1|2013
PA|42|01779798|Pennsylvania|0|10000
RI|44|01219835|Rhode Island|0|10000
SC|45|01779799|South Carolina|0|10000
SD|46|01785534|South Dakota|0|10000
TN|47|01325873|Tennessee|0|10000
TX|48|01779801|Texas|0|10000
UT|49|01455989|Utah|0|10000
VT|50|01779802|Vermont|1|2017
VA|51|01779803|Virginia|0|10000
WA|53|01779804|Washington|1|2011
WV|54|01779805|West Virginia|0|10000
WI|55|01779806|Wisconsin|0|10000
WY|56|01779807|Wyoming|0|10000
AS|60|01802701|American Samoa|0|10000
GU|66|01802705|Guam|0|10000
MP|69|01779809|Commonwealth of the Northern Mariana Islands|0|10000
PR|72|01779808|Puerto Rico|0|10000
UM|74|01878752|U.S. Minor Outlying Islands|0|10000
VI|78|01802710|United States Virgin Islands|0|10000
"

# テキストデータをデータフレームに変換
data <- read.delim(text = data_text, sep = "|", header = FALSE, stringsAsFactors = FALSE)
colnames(data) <- c("StateCode", "state_id", "Code", "StateName","Treatment","ReferencePoint")

# データフレームを表示
print(data)

# id列を主キーとして設定（実際にはデータフレームに主キー設定はありませんが、列名を示す）
geotable <- data %>% select(state_id, everything())

