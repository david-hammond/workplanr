#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' attribute names.
#' Note also that all fields starting with a . are reserved.
#' 
#' @export
NODE_RESERVED_NAMES_CONST <- c( "wp_schedule",
                                "wp_inputs",
                                "clone",
                                "plotStaffSchedule",
                                "plotTeamSchedule",
                                "plotReleaseSchedule",
                                "assignStaff",
                                "addTimeEstimates",
                                "addResponsibilites",
                                "addHolidays",
                                "add_Out_Of_Office",
                                "addRoles",
                                "addPhases",
                                "addProjects",         
                                "addStaff")


#' Create a \code{data.tree} Structure With \code{Nodes}
#' 
#' @description \code{Node} is at the very heart of the \code{data.tree} package. All trees are constructed
#' by tying together \code{Node} objects.
#' 
#' @details Assemble \code{Node} objects into a \code{data.tree}
#' structure and use the traversal methods to set, get, and perform operations on it. Typically, you construct larger tree 
#' structures by converting from \code{data.frame}, \code{list}, or other formats.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @field children A list of child \code{Nodes}
#' @field parent The node's parent \code{Node}
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Node$new(name)}}{Creates a new \code{Node} called \code{name}. Often used to construct the root when creating trees programmatically.}
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node} as a child.}
#'   \item{\code{AddChildNode(node)}}{Adds a \code{Node} as a child.}
#'   \item{\code{AddSibling(name)}}{Creates a new \code{Node} called \code{name} and adds it after this \code{Node} as a sibling.}
#'   \item{\code{AddSiblingNode(sibling)}}{Adds a new \code{Node} after this \code{Node}, as a sibling.}      
#'   \item{\code{RemoveChild(name)}}{Remove the child \code{Node} called \code{name} from a \code{Node} and returns it.}
#'   \item{\code{RemoveAttribute(name, stopIfNotAvailable)}}{Removes attribute called \code{name} from this \code{Node}. Gives an error if \code{stopIfNotAvailable} and the attribute does not exist.}
#'   \item{\code{\link{Climb}(...)}}{Find a node with path \code{...}, where the \code{...} arguments are the \code{name}s of the \code{Node}s, or other field values.}
#'   \item{\code{\link{Navigate}(path)}}{Find a node by relative \code{path}}
#'   \item{\code{\link{FindNode}(name)}}{Find a node with name \code{name}. Especially useful if \code{\link{AreNamesUnique}} is \code{TRUE}}
#'   \item{\code{\link{Get}(attribute, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL, format = NULL, inheritFromAncestors = FALSE, simplify = c(TRUE, FALSE, "array", "regular"))}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Do}(fun, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NUL)}}{Traverses the tree and call fun on each node.}
#'   \item{\code{\link{Set}(..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL)}}{Traverses the tree and assigns the args along the way, recycling the args.}
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE}}{Sort children of a node with respect to an attribute (field, method, active, function)}
#'   \item{\code{\link{Revert}(recursive = TRUE)}}{Revert the sort order of a node}
#'   \item{\code{\link{Prune}(pruneFun)}}{Prune a tree. The pruneFun takes a node as its first argument, and returns TRUE if the node should be kept, FALSE otherwise}
#'
#' }
#' 
#' @section Actives (aka Properties):
#'   
#' \describe{
#'  \item{\code{name}}{Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}}
#'  \item{\code{parent}}{Gets or sets the parent \code{Node} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!}
#'  \item{\code{children}}{Gets or sets the children \code{list} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!}
#'  \item{\code{siblings}}{Returns a list of the siblings of this \code{Node}}
#'  \item{\code{fields}}{Gets the names of the set properties of a \code{Node}}
#'  \item{\code{fieldsAll}}{Gets the names of the set properties of a \code{Node} or any of its sub-Nodes}
#'  \item{\code{isLeaf}}{Returns \code{TRUE} if the \code{Node} is a leaf, \code{FALSE} otherwise}
#'  \item{\code{isRoot}}{Returns \code{TRUE} if the \code{Node} is the root, \code{FALSE} otherwise}
#'  \item{\code{count}}{Returns the number of children of a \code{Node}}
#'  \item{\code{totalCount}}{Returns the total number of \code{Node}s in the tree}
#'  \item{\code{path}}{Returns a vector of mode \code{character} containing the names of the \code{Node}s in the path from the root to this \code{Node}}
#'  \item{\code{pathString}}{Returns a string representing the path to this \code{Node}, separated by backslash}
#'  \item{\code{levelName}}{Returns the name of the \code{Node}, preceded by level times '*'. Useful for printing.}
#'  \item{\code{leafCount}}{Returns the number of leaves are below a \code{Node} }
#'  \item{\code{leaves}}{Returns a list containing all the leaf \code{Node}s }
#'  \item{\code{level}}{Returns an integer representing the level of a \code{Node}. For example, the root has level 1.}
#'  \item{\code{height}}{Returns max(level) of any of the \code{Nodes} of the tree}
#'  \item{\code{averageBranchingFactor}}{Returns the average number of crotches below this \code{Node}}
#'  \item{\code{root}}{Returns the root \code{Node} of a \code{Node}'s tree}
#'  
#' }
#' 
#' @usage # n1 <- Node$new("Node 1")
#'
#' @examples
#' 
#' @seealso For more details see the \code{\link{data.tree}} documentations, or the \code{data.tree} vignette: \code{vignette("data.tree")}
#'
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
#' 
workplan <- R6Class("workplan", 
  public = list(
  wp_inputs = list(staff_name_for_unassigned_work = "unassigned"),
  wp_schedule = list(),
  addStaff = function(staff_name, staff_capacity) {
    self$wp_inputs$staff <-  data.frame(staff_name = as.character(staff_name), 
                                        staff_capacity = as.numeric(staff_capacity),
                                        stringsAsFactors = FALSE)
    private$self_check()
    invisible(self)
  },
  addProjects = function(project_name, project_confirmed,
                         project_start, project_end){
    private$add_projects(project_name, project_confirmed,
                                  project_start, project_end)
    private$self_check()
    invisible(self)
  },
  addPhases = function(project_phase_name){
    project_phase_name <- factor(project_phase_name, project_phase_name, ordered = TRUE)
    self$wp_inputs$project_phases <- data.frame(project_phase_name)
    private$self_check()
    invisible(self)
  },
  addRoles = function(project_role_name){
    project_role_name <- factor(project_role_name, levels = project_role_name, ordered = T)
    self$wp_inputs$project_roles <- data.frame(project_role_name)
    private$self_check()
    invisible(self)
  },
  add_Out_Of_Office = function(staff_name, out_of_office_start, 
                               out_of_office_end, work_related){
    private$staff_name_check(staff_name)
    private$add_OOO(staff_name, out_of_office_start, 
                                  out_of_office_end, work_related)
    private$self_check()
    invisible(self)
  },
  addHolidays = function(date, holiday_name){
    private$add_ph(date, holiday_name)
    private$self_check()
    invisible(self)
  },
  addResponsibilites = function(project_role_name, 
                                project_phase_name, 
                                responsibility_span){
    private$project_role_name_check(project_role_name)
    private$project_phase_name_check(project_phase_name)
    private$add_rr(project_role_name, 
                    project_phase_name, 
                    responsibility_span)
    private$self_check()
    invisible(self)
  },
  addTimeEstimates = function(project_name, 
                              project_phase_name, 
                              time_estimate){
    private$project_name_check(project_name)
    private$project_phase_name_check(project_phase_name)
    private$add_te(project_name, 
                   project_phase_name, 
                   time_estimate)
    private$self_check()
    invisible(self)
  },
  assignStaff = function(staff_name, project_name, project_role_name, staff_contribution){
    private$staff_name_check(staff_name)
    private$project_name_check(project_name)
    private$project_role_name_check(project_role_name)
    for (i in 1:length(staff_name)){
      pos = which(self$wp_inputs$project_assignments$project_name == project_name[i] &
                    self$wp_inputs$project_assignments$project_role_name == project_role_name[i] &
                    self$wp_inputs$project_assignments$staff_name == staff_name[i])
      self$wp_inputs$project_assignments$staff_contribution[pos] <- staff_contribution[i]
      pos <- which(self$wp_inputs$project_unassignments$project_name == project_name[i] &
                     self$wp_inputs$project_unassignments$project_role_name == project_role_name[i])
      self$wp_inputs$project_unassignments$staff_contribution[pos] <- 0
    }
    private$calculate_start_and_end_dates()
    private$get_staff_schedule() #has factor error
    private$get_team_schedule()
    invisible(self)
  },
  plotReleaseSchedule = function(){
    tmp <- self$wp_schedule$release_schedule
    tmp <- tmp %>%
      dplyr::rename(start_date = phase_start, end_date = phase_end)
      
    ref_dates <- apply(data.frame(lubridate::today(), tmp$start_date), 1, max)
    tmp$days_left <- bizdays::bizdays(ref_dates, tmp$end_date, 'normal')
    tmp$mid <- tmp$start_date +(tmp$end_date - tmp$start_date)/2
    tmp$due <- paste0(format(tmp$start_date, "%d/%m"), " (", tmp$days_left, ")")
    tmp$due <- ifelse(tmp$days_left > 0, tmp$due, NA)
    p <- ggplot2::ggplot(tmp, ggplot2::aes(colour=project_phase_name))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x=start_date, 
                                                xend=end_date, 
                                                y=project_name, 
                                                yend=project_name), 
                                   size=10) +
      ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                            date_breaks = '1 month', 
                            expand = c(0,0)) 
    main_title <- paste0("Project Phase Start Dates (days left in each phase) - ", format(lubridate::today(), "%d %B %Y"))
    p <- p + ggrepel::geom_label_repel(data = tmp[!is.na(tmp$due),], 
                                       ggplot2::aes(x = mid, y = project_name, label = due), force = 5,
                                       show.legend = FALSE, size = 3) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                     legend.position = c(0.7,0.9), legend.direction = "horizontal",
                     legend.text= ggplot2::element_text(size=8)) +
      ggplot2::labs(title = main_title, x = "", y = "", colour = "")
    p <- private$plot_public_holidays_and_today(p)
    
    return(p)
  },
  plotTeamSchedule = function(){
    tmp <- self$wp_schedule$team_schedule
    pos <- tmp$workload == 0
    tmp$project_confirmed[pos] <- TRUE
    tmp <- tmp %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(planned_work = sum(workload[project_confirmed]),
                    planned_deficit = ifelse(planned_work > 1, planned_work - 1, 0),
                    potential_work = sum(workload[!project_confirmed]),
                    potential_deficit = ifelse(potential_work  > 1, potential_work - 1, 0),
                    total_potential = potential_work +  potential_deficit) %>%
      dplyr::ungroup()
    pos <- tmp$planned_work > 1
    tmp$planned_work[pos] <- 1
    tmp$potential_deficit[pos] = tmp$potential_deficit[pos] + tmp$potential_work[pos]
    tmp$potential_work[pos] = 0
    
    pos <- tmp$planned_deficit == 0 & (tmp$planned_work + tmp$total_potential) > 1
    tmp$potential_work[pos] <- 1 - tmp$planned_work[pos] 
    tmp$potential_deficit[pos] <- tmp$total_potential[pos] - tmp$potential_work[pos]
    
    tmp <- tmp %>% 
      dplyr::select(-c(project_confirmed, workload, total_potential)) %>% 
      tidyr::gather(group, value, -date) %>%
      dplyr::distinct()
    gg_red <- "#F8766D"
    gg_blue <- "#00BFC4"
    cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
    tmp$group <- gsub("_", " ", tmp$group)
    tmp$group <- private$proper_capitalise(tolower(tmp$group))
    tmp$group = factor(tmp$group, levels = rev(c("Planned Work",
                                                 "Planned Deficit",
                                                 "Potential Work",
                                                 "Potential Deficit")), ordered = T)
    main_title <- paste0("Planned Team Workload - ", format(lubridate::today(), "%d %B %Y"))
    # base layer
    p <- ggplot2::ggplot(tmp, ggplot2::aes(x = date, y = value, fill =  group)) +
      ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
      ggplot2::scale_fill_manual(values = cols) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = main_title, fill = "")  +
      ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                            date_breaks = '1 month', 
                            expand = c(0,0))
    p <- private$plot_public_holidays_and_today(p)
    return(p)
  },
  plotStaffSchedule = function(){
    tmp <- self$wp_schedule$staff_schedule
    tmp <- tmp %>%
      dplyr::filter(date <= max(date[!is.na(workload)]), 
                    !is.na(workload))
    #TODO: make function calc beaks
    bins <- c(0,0.25, 0.5, 0.75, 1, 1.25, 2, max(tmp$workload))
    labels <- c("[0 - 25%]", "[25% - 50%]", "[50% - 75%]", "[75% - 100%]", "[100% - 125%]",
                "[125% - 200%]", paste0("[200% - ", round(100*max(tmp$workload),0), "%]"))
    work_classes <- classInt::classIntervals(tmp$workload, n = length(bins)-1, style = "fixed",
                                             fixedBreaks = bins)
    tmp$workload = cut(tmp$workload, bins, labels = labels)
    tmp$workload = forcats::fct_rev(tmp$workload)
    main_title <- paste0("Planned Staff Workload - ", format(lubridate::today(), "%d %B %Y"))
    # labels = scales::percent, 
    p <- ggplot2::ggplot(data = tmp, 
                         ggplot2::aes(date, staff_name, fill = workload)) +
      ggplot2::geom_tile(alpha = 0.6) +
      ggplot2::scale_fill_manual(values = 
                                   rev(RColorBrewer::brewer.pal(n = max(length(unique(tmp$workload)),3), 
                                                                "Reds")), 
                                 name = 'Workload', na.value = "white") +
      ggplot2::labs(x='', y = '', title = main_title) +
      ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                            date_breaks = '1 month', 
                            expand = c(0,0))
    project_labels <- tmp %>% 
      dplyr::select(date, staff_name, project_name, workload) %>%
      dplyr::distinct() %>% 
      dplyr::filter(!is.na(project_name))
    p <- p + ggrepel::geom_text_repel(data = project_labels, 
                                      ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                      size = 3, hjust = 1)   
      
    
    #add leave
    
    leave <- tmp %>%
      dplyr::group_by(staff_name, id_out_of_office, out_of_office) %>%
      dplyr::summarise(start = min(date), end = max(date), 
                       workload = tmp$workload[1]) %>%
      dplyr::filter(!is.na(out_of_office))
    
    p <- p + ggplot2::geom_segment(data = leave, ggplot2::aes(x=start, 
                                                              xend=end, 
                                                              y=staff_name, 
                                                              yend=staff_name, colour = out_of_office), size=2, alpha = 0.6)
    p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=start, 
                                                            y=staff_name, colour = out_of_office), size=3,
                                 show.legend = FALSE)
    p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=end, 
                                                            y=staff_name, colour = out_of_office), size=3,
                                 show.legend = FALSE)
    p <- p + ggplot2::labs(fill ="Workload" ,colour="Out of Office")
    p <- p + ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))
    p <- private$plot_public_holidays_and_today(p)
    return(p)
  }),
  private = list(
    self_check = function(){
      inputs <- c("staff",
                  "projects",
                  "project_phases",
                  "project_roles",
                  "out_of_office",
                  "public_holidays",
                  "roles_responsibilities",
                  "time_estimates")
      tbd <- setdiff(inputs, names(self$wp_inputs))
      if(length(tbd) > 0){
        message(paste0("You still need to input:\n", paste(tbd, collapse = "\n"), 
                       "\nbefore project schedules can be calculated"))
      }else{
        private$create_project_assignments()
        message(paste0("Your schedule is ready to be calculated. To begin, use assignStaff()"))
      }
      invisible(self)
    },
    add_projects = function(project_name, project_confirmed,
                            project_start, project_end){
      project_name <- as.character(project_name)
      project_confirmed <- as.logical(project_confirmed)
      project_start <- as.Date(project_start)
      project_end <- as.Date(project_end)
      self$wp_inputs$projects <- data.frame(project_name, project_confirmed,
                                            project_start, project_end)
      self$wp_inputs$projects <- self$wp_inputs$projects %>%
        dplyr::arrange(project_end) %>% 
        dplyr::distinct() %>%
        dplyr::mutate(project_name = factor(project_name, levels = rev(project_name),
                                            ordered = TRUE))
      invisible(self)
    },
    add_OOO = function(staff_name, out_of_office_start, 
                       out_of_office_end, work_related){
      id_out_of_office <- 1:length(staff_name)
      staff_name <- as.character(staff_name)
      out_of_office_start <- as.Date(out_of_office_start)
      out_of_office_end <- as.Date(out_of_office_end)
      work_related <- as.logical(work_related)
      self$wp_inputs$out_of_office <- data.frame(id_out_of_office,
                                                 staff_name,
                                                 out_of_office_start,
                                                 out_of_office_end,
                                                 work_related,
                                                 stringsAsFactors = FALSE)
      invisible(self)
    },
    add_ph = function(date, holiday_name){
      date <- as.Date(date)
      holiday_name <- as.character(holiday_name)
      self$wp_inputs$public_holidays <- data.frame(date, holiday_name)
      invisible(self)
    },
    add_rr = function(project_role_name, 
                      project_phase_name, 
                      responsibility_span){
      project_role_name <- as.character(project_role_name)
      project_phase_name <- as.character(project_phase_name)
      responsibility_span <- as.logical(responsibility_span)
      self$wp_inputs$roles_responsibilities <- data.frame(project_role_name, 
                                                         project_phase_name, 
                                                         responsibility_span)
      self$wp_inputs$roles_responsibilities$project_role_name <- factor(self$wp_inputs$roles_responsibilities$project_role_name,
                                                                        levels = self$wp_inputs$project_roles$project_role_name,
                                                                        ordered = T)
      self$wp_inputs$roles_responsibilities$project_phase_name <- factor(self$wp_inputs$roles_responsibilities$project_phase_name,
                                                                         levels = self$wp_inputs$project_phases$project_phase_name,
                                                                         ordered = T)
      invisible(self)
    },
    add_te = function(project_name, 
                      project_phase_name, 
                      time_estimate){
      project_name <- as.character(project_name)
      project_phase_name <- as.character(project_phase_name)
      time_estimate <- as.numeric(time_estimate)
      self$wp_inputs$time_estimates <- data.frame(project_name, 
                                                  project_phase_name, 
                                                  time_estimate)
      self$wp_inputs$time_estimates$project_name <- factor(self$wp_inputs$time_estimates$project_name,
                                                           levels = levels(self$wp_inputs$projects$project_name),
                                                           ordered = T)
      self$wp_inputs$time_estimates$project_phase_name <- factor(self$wp_inputs$time_estimates$project_phase_name,
                                                                 levels = self$wp_inputs$project_phases$project_phase_name,
                                                                 ordered = T)
      invisible(self)
    },
    create_project_assignments = function(){
      project_assignments <- expand.grid(staff_name = self$wp_inputs$staff$staff_name, 
                                         project_role_name = self$wp_inputs$project_roles$project_role_name,
                                         project_phase_name = self$wp_inputs$project_phases$project_phase_name, 
                                         project_name = self$wp_inputs$projects$project_name,
                                         KEEP.OUT.ATTRS = FALSE)
      
      project_assignments$staff_contribution <- 0
      project_assignments <- project_assignments %>% 
        dplyr::left_join(self$wp_inputs$time_estimates) %>%
        dplyr::filter(abs(time_estimate) > 0) %>% 
        dplyr::select(-time_estimate)
      
      project_assignments <- project_assignments %>%
        dplyr::left_join(self$wp_inputs$roles_responsibilities) %>%
        dplyr::filter(responsibility_span == 1) %>% 
        dplyr::select(-responsibility_span)
      
      self$wp_inputs$project_assignments <- data.frame(staff_name = as.character(project_assignments$staff_name),
                                                     project_role_name = project_assignments$project_role_name,
                                                     project_phase_name = project_assignments$project_phase_name,
                                                     project_name = project_assignments$project_name,
                                                     staff_contribution = project_assignments$staff_contribution)
      
      project_assignments <- project_assignments %>%
        dplyr::mutate(staff_name = self$wp_inputs$staff_name_for_unassigned_work,
                      staff_contribution = 100) %>%
        dplyr::distinct()
      
      self$wp_inputs$project_unassignments <- data.frame(staff_name = as.character(project_assignments$staff_name),
                                                       project_role_name = project_assignments$project_role_name,
                                                       project_phase_name = project_assignments$project_phase_name,
                                                       project_name = project_assignments$project_name,
                                                       staff_contribution = project_assignments$staff_contribution)
      invisible(self)
    },
    calculate_start_and_end_dates = function(){
      tmp <- self$wp_inputs$projects %>%
        dplyr::left_join(self$wp_inputs$time_estimates) %>%
        dplyr::filter(time_estimate != 0)
      
      cals <- bizdays::create.calendar('normal', 
                                       weekdays = c('saturday', 'sunday'), 
                                       holidays = self$wp_inputs$public_holidays$date,
                                       start.date = min(tmp$project_start) - 2*max(abs(tmp$time_estimate)), 
                                       end.date = max(tmp$project_end) + 2*max(abs(tmp$time_estimate)))
      
      #post end activities
      pos <- tmp$time_estimate > 0
      if(sum(pos) > 0){ #if there are pre and post phases
        post_project_end_phases <- tmp[pos,]
        post_project_end_phases <- post_project_end_phases %>%
          dplyr::group_by(project_name) %>%
          dplyr::mutate(day_shift = match(project_phase_name, rev(self$wp_inputs$project_phases$project_phase_name)) - 1,
                        time_from_project_end = cumsum(time_estimate) + day_shift,
                        time_from_phase_end = -time_estimate,
                        phase_end = bizdays::offset(project_end, time_from_project_end, 'normal'),
                        phase_start = bizdays::offset(phase_end, time_from_phase_end, 'normal')) %>%
          dplyr::ungroup()
        
        pre_project_end_phases <- tmp[!pos,]
        pre_project_end_phases <- pre_project_end_phases %>%
          dplyr::group_by(project_name) %>% 
          dplyr::arrange(project_name, dplyr::desc(project_phase_name)) %>%
          dplyr::mutate(day_shift = match(project_phase_name, rev(self$wp_inputs$project_phases$project_phase_name)) - 1,
                        time_from_project_end = cumsum(time_estimate) - day_shift,
                        time_from_phase_end = -time_estimate,
                        phase_start = bizdays::offset(project_end, time_from_project_end, 'normal'),
                        phase_end = bizdays::offset(phase_start, time_from_phase_end, 'normal')) %>%
          dplyr::ungroup()
        tmp <- rbind(pre_project_end_phases, post_project_end_phases[,names(pre_project_end_phases)])
      }else{
        tmp <- tmp %>%
          dplyr::group_by(project_name) %>% 
          dplyr::arrange(project_name, dplyr::desc(project_phase_name)) %>%
          dplyr::mutate(time_from_project_end = cumsum(time_estimate),
                        time_from_phase_end = -time_estimate,
                        phase_start = bizdays::offset(project_end, time_from_project_end, 'normal'),
                        phase_end = bizdays::offset(phase_start, time_from_phase_end, 'normal')) %>%
          dplyr::ungroup()
      }
      
      #allow for extra time if numbers dont add up
      pos <- tmp$project_phase_name == self$wp_inputs$project_phases$project_phase_name[1] &
        tmp$phase_start > tmp$project_start
      tmp$phase_start[pos] <-  tmp$project_start[pos]
      
      tmp <- tmp %>%
        dplyr::select(project_name, project_confirmed, project_phase_name, phase_start, phase_end) %>%
        tidyr::gather(date_type, date, -c(project_name, project_confirmed, project_phase_name))
      
      self$wp_schedule$release_schedule <- tmp %>%
        tidyr::spread(date_type, date) %>%
        dplyr::select(project_name, project_confirmed, project_phase_name, phase_start, phase_end)
      
      tmp <- tmp %>%
        dplyr::group_by(project_name, project_confirmed, project_phase_name) %>%
        padr::pad() %>%
        dplyr::ungroup()  %>% 
        dplyr::mutate(date = as.Date(date))
      
      tmp <- tmp %>%
        dplyr::left_join(rbind(self$wp_inputs$project_assignments, 
                               self$wp_inputs$project_unassignments)) %>%
        dplyr::filter(staff_contribution > 0) %>%
        dplyr::mutate(staff_name = as.character(staff_name)) %>%
        dplyr::left_join(self$wp_inputs$staff) %>%
        dplyr::mutate(staff_capacity = ifelse(is.na(staff_capacity), 0, staff_capacity))
      
      self$wp_schedule$schedule <- tmp %>%
        dplyr::left_join(as.data.frame(self$wp_inputs$public_holidays)) %>%
        dplyr::select(date, project_name, project_confirmed,  
                      project_role_name, project_phase_name, staff_name, staff_capacity,
                      staff_contribution, holiday_name) %>%
        dplyr::arrange(date)
      invisible(self)
    },
    get_staff_schedule = function(){
      staff_schedule <-  self$wp_schedule$schedule %>%
        dplyr::group_by(date, staff_name) %>%
        dplyr::summarise(workload = sum(staff_contribution)/
                           max(c(staff_capacity, max(self$wp_inputs$staff$staff_capacity)), na.rm = T)) %>%
        dplyr::ungroup() 
      projects <- self$wp_schedule$release_schedule %>%
        dplyr::select(project_name, phase_start) %>%
        dplyr::rename(date = phase_start) %>%
        dplyr::group_by(project_name) %>%
        dplyr::filter(date == min(date)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(project_name = paste(project_name, collapse = ", ")) %>%
        dplyr::ungroup() 
      staff_schedule <- staff_schedule %>%
        dplyr::left_join(projects)

      staff_schedule <- staff_schedule %>%
        dplyr::left_join(self$wp_inputs$public_holidays)
      
      leave <- as.data.frame(self$wp_inputs$out_of_office)
      leave <- leave %>% 
        tidyr::gather(date_type, date, -c(id_out_of_office, staff_name, work_related)) %>%
        dplyr::group_by(id_out_of_office, staff_name, work_related) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        padr::pad() %>% 
        dplyr::select(-date_type) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date = as.character(date)) %>%
        dplyr::rename(out_of_office = work_related) %>%
        dplyr::mutate(out_of_office = ifelse(as.numeric(out_of_office) == 1, "Work", "Vacation"),
                      date = as.Date(date)) 
      staff_schedule <- staff_schedule %>%
        dplyr::full_join(leave) 
      staff_schedule <- staff_schedule %>%
        dplyr::distinct()
      staff_schedule$staff_name <- factor(staff_schedule$staff_name, 
                                          levels = c(as.character(rev(self$wp_inputs$staff$staff_name)), 
                                                     as.character(unique(self$wp_inputs$project_unassignments$staff_name))),
                                          ordered = TRUE)
      
      self$wp_schedule$staff_schedule <- data.frame(date = as.Date(staff_schedule$date),
                                                 staff_name = staff_schedule$staff_name,
                                                 workload = as.numeric(staff_schedule$workload),
                                                 project_name = as.character(staff_schedule$project_name),
                                                 id_out_of_office = as.numeric(staff_schedule$id_out_of_office),
                                                 out_of_office = as.character(staff_schedule$out_of_office),
                                                 holiday_name = as.character(staff_schedule$holiday_name))
      
      invisible(self)
    },
    get_team_schedule = function(){
      team_capacity <- sum(self$wp_inputs$staff$staff_capacity, na.rm = T)
      tmp <- self$wp_schedule$schedule %>% 
        dplyr::group_by(date, project_confirmed) %>% 
        dplyr::summarise(staff_contribution = sum(staff_contribution, na.rm=TRUE),
                         workload = round(staff_contribution/team_capacity,2)) %>%
        dplyr::select(-staff_contribution) %>%
        dplyr::ungroup() 
      self$wp_schedule$team_schedule <- data.frame(date = as.Date(tmp$date),
                                     project_confirmed = as.logical(tmp$project_confirmed),
                                     workload = as.numeric(tmp$workload))
      invisible(self)
      },
    staff_name_check = function(staff_name){
      testthat::expect(length(setdiff(staff_name, self$wp_inputs$staff$staff_name)) == 0 ,
                       failure_message = paste(paste0(setdiff(staff_name, self$wp_inputs$staff$staff_name), collapse = ", "),
                                               "Not in your staff list, please check spelling"))
    },
    project_name_check = function(project_name){
      testthat::expect(length(setdiff(project_name, self$wp_inputs$projects$project_name)) == 0 ,
                       failure_message = paste(paste0(setdiff(project_name, self$wp_inputs$projects$project_name), collapse = ", "), 
                                               "Not in your project list, please check spelling"))
    },
    project_phase_name_check = function(project_phase_name){
      testthat::expect(length(setdiff(project_phase_name, self$wp_inputs$project_phases$project_phase_name)) == 0 ,
                       failure_message = paste(paste0(setdiff(project_phase_name, self$wp_inputs$project_phases$project_phase_name), collapse = ", "), 
                                               "Not in your phase list, please check spelling"))
    },
    project_role_name_check = function(project_role_name, workplan){
      testthat::expect(length(setdiff(project_role_name, self$wp_inputs$project_roles$project_role_name)) == 0 ,
                       failure_message = paste(paste0(setdiff(project_role_name, self$wp_inputs$project_roles$project_role_name), collapse = ", "), 
                                               "Not in your role list, please check spelling"))
    },
    proper_capitalise = function(string){
      gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(string), perl=TRUE)
    },
    plot_public_holidays_and_today = function(p){
      public_holidays <- self$wp_inputs$public_holidays
      p <- p + ggplot2::geom_vline(xintercept = public_holidays$date,
                                   linetype = "dashed", colour = grey(0.5), alpha = 0.6) +
        ggplot2::geom_vline(xintercept = lubridate::today(), colour = "red", linetype = "dashed")
      return(p)
    }
  )
)

