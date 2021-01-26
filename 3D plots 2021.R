install.packages("scatterplot3d") # Install
library("scatterplot3d")
scatterplot3d(fish[,1:3], angle = 50)




install.packages("plotly")
library(plotly)

lat$tissue[which(lat$tissue == 0)] <- 'liver'
lat$tissue[which(lat$tissue == 1)] <- 'muscle'
lat$tissue <- as.factor(lat$tissue)

fig <- plot_ly(latpuppies, x = ~C, y = ~S, z = ~N, color= ~tissue, colors = c('#d95f02', '#1f78b4', '#b2df8a'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(title='LAT Puppies', scene = list(xaxis = list(title ='Carbon'),
                                   yaxis = list(title = 'Sulfur'),
                                   zaxis = list(title = 'Nitrogen')))

fig


fig2 <- plot_ly(latpups, x = ~C, y = ~S, z = ~N, color= ~tissue, colors = c('#d95f02', '#1f78b4', '#b2df8a'))
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(title='LAT Pups',scene = list(xaxis = list(title = 'Carbon'),
                                   yaxis = list(title = 'Sulfur'),
                                   zaxis = list(title = 'Nitrogen')))

fig2


fig3 <- plot_ly(lattrophy, x = ~C, y = ~S, z = ~N, color= ~region, colors = c('#d95f02', '#1f78b4', '#b2df8a'))
fig3 <- fig3 %>% add_markers()
fig3 <- fig3 %>% layout(title='LAT Trophy',scene = list(xaxis = list(title = 'Carbon'),
                                    yaxis = list(title = 'Sulfur'),
                                    zaxis = list(title = 'Nitrogen')))

fig3



