#include <iostream>
#include "bmi.hxx"
namespace wrf_hydro = bmi;

int main() {
  std::cout << "--- Start in C++ ---" << std::endl;

  int res;
  res = wrf_hydro::initialize("");

  double current_time, end_time;
  res = wrf_hydro::get_start_time(&current_time);
  res = wrf_hydro::get_end_time(&end_time);

  while (current_time < end_time) {
    res = wrf_hydro::update();
    res = wrf_hydro::get_current_time(&current_time);
    if ((int(current_time) % 10) == 0)
      std::cout << "time = " << current_time << std::endl;
  }

  res = wrf_hydro::finalize();
  std::cout << "--- Finished running test driver ---" << std::endl;
  return 0;
}
