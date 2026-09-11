library(nycflights13)
library(tidyverse)
flights
glimpse(flights)


flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# filter ------------------------------------------------------------------

flights |> 
  filter(dep_delay > 120)

flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month %in% c(1, 2))


flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

flights |> 
  distinct(origin, dest)

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |> 
  distinct(dest)

flights |>
  count(origin, dest, sort = TRUE)

# mutate ------------------------------------------------------------------
# generamos dos nuevas columnas y las ordenamos xa q aparezcan adelante en la tabla
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

# seleccionamos los datos que queremos que nos aparezcan seleccionando cada una
flights |> 
  select(year, month, day)

# o si sabemos como estan organizadas desde la primera hasta la que queremos
flights |> 
  select(year:day)

# o si queremos excluir ese grupo de columnas
flights |> 
  select(!year:day)

flights |> 
  select(tail_num = tailnum)

# para saber cuales son character
glimpse(flights)
flights |> 
  select(where(is.character))

# rename ------------------------------------------------------------------

flights |> 
  rename(tail_num = tailnum)

# relocate ----------------------------------------------------------------

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)

flights |> 
  relocate(starts_with("arr"), .before = dep_time)

# the pipe ----------------------------------------------------------------
# EL ORDEN DE LOS COMANDOS IMPORTA
flights |> 
  filter(dest == "IAH") |> #achica la cantidad de filas
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

# group by ----------------------------------------------------------------
# para agrupar los datos segun meses
flights |> 
  group_by(month)

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

flights |> 
  group_by(origin, month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  ) |> print(n=36)
flights

# slice
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

daily <- flights |>  
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

daily |> 
  ungroup()



# by ----------------------------------------------------------------------

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )
### EXERCISES ###
#1
flights |> 
  group_by(carrier) |> 
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) |> 
  arrange(desc(avg_delay))

#2
flights |> 
  group_by(dest) |> 
  filter(dep_delay == max(dep_delay, na.rm = TRUE)) |> 
  relocate(dest, dep_delay)

#4
# si pongo un numero negativo a slice_min() y otros se va a eliminar el ultimo valor y me devolvera todos los demas valores

#5
#el termino count agrupa segun un grupo que le asignemos, los cuenta y luego los desagrupa. El termino sort lo que hace es ordenar los valores de mayor a menor valor