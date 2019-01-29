require(tidyverse)

# Get the data
url1 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv'
url2 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/fluid_milk_sales.csv'

milkProductsData <- read_csv(url1)
milkSalesData <- read_csv(url2)

# Get the data from the fluid_milk_sales dataset to get production
milkSales_df <- milkSalesData %>% 
  filter(milk_type=="Total Production") %>% 
  mutate(millions_of_pounds = pounds/1000000)

# left_join this to milkProductsData
milkProdCons_df <- left_join(milkSales_df, milkProductsData,
                             by = c("year"))

# Make the products into one column
milkProdRatio_df <- milkProdCons_df %>% 
  select(year:dry_whey) %>% 
  gather(product_type, ave_consump, fluid_milk:dry_whey) %>% 
  mutate(product_type = case_when(
                            str_detect(product_type, "fluid_milk") ~ "Milk",
                            str_detect(product_type, "fluid_yogurt") ~ "Yogurt",
                            # str_detect(product_type, "butter") ~ "Butter",
                            str_detect(product_type, "cheese_american") ~ "American Cheese",
                            str_detect(product_type, "cheese_other") ~ "Other Cheese",
                            str_detect(product_type, "cheese_cottage") ~ "Cottage Cheese",
                            str_detect(product_type, "evap_cnd_canned_whole_milk") ~ "Evaporated/Canned Whole Milk",
                            str_detect(product_type, "evap_cnd_bulk_whole_milk") ~ "Evaporated/Canned Bulk Whole Milk",
                            str_detect(product_type, "evap_cnd_bulk_and_can_skim_milk") ~ "Evaporated/Canned Bulk and Can Skim Milk",
                            str_detect(product_type, "frozen_ice_cream_regular") ~ "Regular Ice Cream",
                            str_detect(product_type, "frozen_ice_cream_reduced_fat") ~ "Reduced-Fat Ice Cream",
                            str_detect(product_type, "frozen_sherbet") ~ "Sherbet",
                            str_detect(product_type, "frozen_other") ~ "Other Frozen Milk Product",
                            str_detect(product_type, "dry_whole_milk") ~ "Dry Whole Milk",
                            str_detect(product_type, "dry_nonfat_milk") ~ "Dry Nonfat Milk",
                            str_detect(product_type, "dry_buttermilk") ~ "Dry Buttermilk",
                            str_detect(product_type, "dry_whey") ~ "Dry Whey/Milk Protein",
                            TRUE ~ NA_character_)) %>% 
  mutate(product_form = case_when(
    str_detect(product_type, "Butter") ~ "Butter/Cheese",
    str_detect(product_type, "Cheese") ~ "Butter/Cheese",
    str_detect(product_type, "Yogurt") ~ "Fluid",
    str_detect(product_type, "Canned") ~ "Canned",
    str_detect(product_type, "Ice Cream") ~ "Frozen",
    str_detect(product_type, "Sherbet") ~ "Frozen",
    str_detect(product_type, "Frozen") ~ "Frozen",
    str_detect(product_type, "dry") ~ "Dry",
    str_detect(product_type, "Milk") ~ "Fluid",
    TRUE  ~ NA_character_
  ))

milkRatio <- milkProdRatio_df %>%
  mutate(prop = ((ave_consump*10000)/millions_of_pounds)) 
  # select(-c("milk_type","pounds"))

# Filter required rows for the labels
prodLabs <- milkRatio[milkRatio$year == 2016, ] # since 2016 gets plotted on the rightmost
prodLabs$label <- prodLabs$product_type


# Plot
g <- ggplot(milkRatio,
            aes(x = year, y = log10(prop)
                , color = product_type
                , linetype = product_form
                , label = product_type
                )) +
  geom_text_repel(data=prodLabs
            , aes(label=label)
            , size=4
            , direction = "both"
  ) +
  geom_line(stat="identity", size = 1.1
            # ,show.legend = FALSE
  ) +
  labs(x = "Year", y = "Ratio of ave. personal consumption to total milk production (.001 lb)"
       , linetype="Form"
       , subtitle = "*Ratio log-transformed"
       ) +
  guides(color=FALSE) +
  ggtitle("Consumption-to-Production ratio of US Dairy Products") +
  theme_minimal(base_size = 12)

print(g)

ggsave("milkProducts.png"
       ,plot = g
       ,width=9
       ,height=7)
