rm(list=ls())
# Schelling model is situated on a grid and in simple form one cell of a grid represents three states: uninhabited,
# inhabited by a member of one group, inhabitd by a member of second group: 0, 1, 2. This code presents simple Schelling model.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# set up ------------------------------------------------------------------
group  <- c(rep(0, 51*51-number), rep(1, number/2), rep(2, number/2))
grid <- matrix(sample(group, 2601, replace = F), ncol = 51)

alike_preference  <- .60

happiness_tracker <- c()


# function to navigate 8 cells around -------------------------------------
get_neighbors<-function(coords) {
    n<-c()
    for (i in c(1:8)) {

        if (i == 1) {
            x<-coords[1] + 1
            y<-coords[2]
        }

        if (i == 2) {
            x<-coords[1] + 1
            y<-coords[2] + 1
        }

        if (i == 3) {
            x<-coords[1]
            y<-coords[2] + 1
        }

        if (i == 4) {
            x<-coords[1] - 1
            y<-coords[2] + 1
        }

        if (i == 5) {
            x<-coords[1] - 1
            y<-coords[2]
        }

        if (i == 6) {
            x<-coords[1] - 1
            y<-coords[2] - 1
        }

        if (i == 7) {
            x<-coords[1]
            y<-coords[2] - 1
        }

        if (i == 8) {
            x<-coords[1] + 1
            y<-coords[2] - 1
        }

        if (x < 1) {
            x<-51
        }
        if (x > 51) {
            x<-1
        }
        if (y < 1) {
            y<-51
        }
        if (y > 51) {
            y<-1
        }
        n<-rbind(n,c(x,y))
    }
    n
}


# Schelling's model -------------------------------------------------------
matrices_list <- list()

for (t in 1:100) {
    happy_cells <- c()
    unhappy_cells<-c()  

    for (j in c(1:51)) {
        for (k in c(1:51)) {
            current<-c(j,k)
            value<-grid[j,k] 
            if (value > 0) {
                like_neighbors<-0
                all_neighbors<-0
                neighbors<-get_neighbors(current)
                for (i in c(1:nrow(neighbors))){
                    x<-neighbors[i,1]
                    y<-neighbors[i,2]
                    if (grid[x,y] > 0) {
                        all_neighbors<-all_neighbors + 1
                    }
                    if (grid[x,y] == value) {
                        like_neighbors<-like_neighbors + 1
                    }
                }
                if (is.nan(like_neighbors / all_neighbors)==FALSE) {
                    if ((like_neighbors / all_neighbors) < alike_preference) {
                        unhappy_cells<-rbind(unhappy_cells,c(current[1],current[2]))
                    }
                    else {
                        happy_cells<-rbind(happy_cells,c(current[1],current[2]))
                    }
                }

                else {
                    happy_cells<-rbind(happy_cells,c(current[1],current[2]))
                }
            }
        }
    }


    happiness_tracker<-append(happiness_tracker,length(happy_cells)/(length(happy_cells) + length(unhappy_cells)))

    # move unhappies to spaces that are not occupied
    rand<-sample(nrow(unhappy_cells))
    for (i in rand) {
        mover<-unhappy_cells[i,]
        mover_val<-grid[mover[1],mover[2]]
        move_to<-c(sample(1:51,1),sample(1:51,1))
        move_to_val<-grid[move_to[1], move_to[2]]
        while (move_to_val > 0 ){
            move_to<-c(sample(1:51,1),sample(1:51,1))
            move_to_val<-grid[move_to[1],move_to[2]]
        }
        grid[mover[1],mover[2]]<-0
        grid[move_to[1],move_to[2]]<-mover_val
    }

matrices_list[[t]] <- grid
}


# prepare data frame to ggplot2 -------------------------------------------
df <- as.data.frame(do.call('rbind', matrices_list))
df$id <- rep(1:51, 100)
df$iteration <- rep(1:100, each = 51)

require(reshape2)

m_grid <- melt(df, id.var=c("id", "iteration"))

m_grid


# animate the plot with gganimate -----------------------------------------
require(gganimate)
p1 <- ggplot(m_grid, aes(x=variable,
                         y=id,fill=factor(value))) +
               geom_tile() 

p2 <- p1 + transition_states(iteration,
                  transition_length = 2,
                  state_length = 1)

p2
