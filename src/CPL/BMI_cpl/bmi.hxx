// The Basic Model Interface (BMI) C++ specification.
//
// This language specification is derived from the Scientific
// Interface Definition Language (SIDL) file bmi.sidl located at
// https://github.com/csdms/bmi.

#ifndef BMI_HXX
#define BMI_HXX

#include <vector>

namespace bmi {

  const int BMI_SUCCESS = 0;
  const int BMI_FAILURE = 1;

  const int MAX_COMPONENT_NAME = 2048;
  const int MAX_VAR_NAME = 2048;
  const int MAX_UNITS_NAME = 2048;
  const int MAX_TYPE_NAME = 2048;


  // hacky fix: extern works here, rename functions to c_bind name
  extern "C" {

  // class Bmi {
  //   public:
      // Model control functions.
       int initialize(std::string config_file);
       int update();
       void UpdateUntil(double time);
       int finalize();

      // Model information functions.
       std::string GetComponentName();
       int GetInputItemCount();
       int GetOutputItemCount();
       std::vector<std::string> GetInputVarNames();
       std::vector<std::string> GetOutputVarNames();

      // Variable information functions
       int GetVarGrid(std::string name);
       std::string GetVarType(std::string name);
       std::string GetVarUnits(std::string name);
       int GetVarItemsize(std::string name);
       int GetVarNbytes(std::string name);
       std::string GetVarLocation(std::string name);

       int get_current_time(double *time); // double GetCurrentTime();
       int get_start_time(double *time); // double GetStartTime();
       int get_end_time(double *time); // double GetEndTime();
       std::string GetTimeUnits();
       double GetTimeStep();

      // Variable getters
       void GetValue(std::string name, void *dest);
       void *GetValuePtr(std::string name);
       void GetValueAtIndices(std::string name, void *dest, int *inds, int count);

      // Variable setters
       void SetValue(std::string name, void *src);
       void SetValueAtIndices(std::string name, int *inds, int count, void *src);

      // Grid information functions
       int GetGridRank(const int grid);
       int GetGridSize(const int grid);
       std::string GetGridType(const int grid);

       void GetGridShape(const int grid, int *shape);
       void GetGridSpacing(const int grid, double *spacing);
       void GetGridOrigin(const int grid, double *origin);

       void GetGridX(const int grid, double *x);
       void GetGridY(const int grid, double *y);
       void GetGridZ(const int grid, double *z);

       int GetGridNodeCount(const int grid);
       int GetGridEdgeCount(const int grid);
       int GetGridFaceCount(const int grid);

       void GetGridEdgeNodes(const int grid, int *edge_nodes);
       void GetGridFaceEdges(const int grid, int *face_edges);
       void GetGridFaceNodes(const int grid, int *face_nodes);
       void GetGridNodesPerFace(const int grid, int *nodes_per_face);
  };
}

#endif
