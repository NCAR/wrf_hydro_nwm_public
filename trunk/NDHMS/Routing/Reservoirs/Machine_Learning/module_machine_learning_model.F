! This module defines and instantiates objects
! for a machine learning model that will be used
! by machine learning type reservoirs. These model
! objects are not sub-objects of machine learning
! reservoirs but instead independent and match a
! model ID for a given model to the model that will
! be used by a reservoir. This module reads in the
! model parameters from its given NetCDF file and
! then constructs the model that will be called
! by a machine learning reservoir's release function.

module module_machine_learning_model

    use netcdf
    use module_reservoir_utilities
    implicit none

    type :: machine_learning_model_container
        class (machine_learning_model_struct), pointer :: ptr
    end type

    class (machine_learning_model_container), allocatable, dimension(:) :: machine_learning_models
    integer, allocatable, dimension(:) :: machine_learning_model_ids

    integer :: number_of_unique_ml_models

    type NN_reservoir_in_out
        REAL :: previous_time_release
        REAL :: inflow
        REAL :: current_storage
        REAL :: x_day_of_year
        REAL :: y_day_of_year
    end type NN_reservoir_in_out

    type NN_node
        real, allocatable, dimension (:,:) :: link_weights
        real, allocatable, dimension (:,:) :: link_bias
        integer, dimension (10) :: link_activation_functions ! FIXME, I had a hard time setting this into an allocatable dimension; so I had to fix it to a constant maximum number of layer = 10
    end type NN_node


    type :: machine_learning_model_struct

        integer :: model_number
        real :: normalized_simulated_release
        real :: maximum_release, minimum_release

        integer, allocatable, dimension (:)  :: number_of_nodes ! is just a function of layer; number of computational nodes (i.e., neurons); first element represents the number of inputs to the model
        real, allocatable, dimension (:)     :: minimum_of_features_reshaped
        real, allocatable, dimension (:)     :: maximum_of_features_reshaped
        real, dimension  (1000,1) :: nn_sum_matrix ! The sum of matrix calculation(Wx+b) before applying transfer function!
        real, dimension  (1000,1) :: reshaped_nn_sum_matrix_activated
        real, dimension (1000) :: reshaped_nn_sum_matrix ! a reshaped version of the "nn_sum_matrix"
        real, dimension (1000) :: nn_sum_matrix_activated ! the output of the NN by applying the transfer function on "nn_sum_matrix"

        type (NN_node), dimension (10)  :: model_parameters
        type (NN_reservoir_in_out)  :: nn_inputs ! the dimension should apparently represent the number of inputs
        type (NN_reservoir_in_out)  :: normalized_nn_inputs
        type (NN_reservoir_in_out) :: reshaped_nn_inputs

    contains

        procedure :: init => machine_learning_model_init
        procedure :: destroy => machine_learning_model_destroy
        procedure :: run_model => run_machine_learning_model

    end type machine_learning_model_struct

contains


    !Machine Learning Model Constructor
    subroutine machine_learning_model_init(this, model_number)
        use netcdf
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: model_number

        real, allocatable, dimension (:,:)   :: bias ! Bias has one dimension less than that for the weights!
        real, allocatable, dimension (:,:)   :: weights ! for each layer, there is a two-dimension matrix
        real, allocatable, dimension (:,:,:) :: reshaped_weights !
        real, allocatable, dimension (:,:)   :: reshaped_weights_1st
        real, allocatable, dimension (:,:)   :: reshaped_weights_2nd
        real, allocatable, dimension (:,:)   :: reshaped_weights_3rd
        real, allocatable, dimension (:,:)   :: reshaped_weights_4th
        real, allocatable, dimension (:)     :: minimum_of_features ! is a function of the size of the inputs
        real, allocatable, dimension (:)     :: maximum_of_features	! is a function of the size of the inputs
        integer, allocatable, dimension (:)  :: activation_functions ! is just a function of layer

        integer :: number_of_inputs ! number of inputs, though all might not be used as inputs to the ML model
        integer :: nodes_per_layer  ! number of neurons which is a function of layers and model number
        integer :: number_of_layers ! number of model layers, which is actually the number of layers + 1 for matrix dimensions
        integer :: node_index
        integer :: ncid, var_id
        integer :: status       ! Status of reading NetCDF

        ! If model_number is 1, then open ACF basin NetCDF. Else if model_number is 2, then open Upper Colorado basin NetCDF

		if (model_number == 1) then
			status = nf90_open(path = "ML_MODEL_PARM_ACF.nc", mode = nf90_nowrite, ncid = ncid)               ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status, "ML_MODEL_PARM_ACF.nc")


		else if (model_number == 2) then
			status = nf90_open(path = "ML_MODEL_PARM_CH.nc", mode = nf90_nowrite, ncid = ncid)                ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status, "ML_MODEL_PARM_CH.nc")

		end if




        this%model_number = model_number

        ! Call subroutines to read in model parameters from the corresponding NetCDF file
        call read_machine_learning_model_netcdf_real_2D_parameters(ncid, "bias", var_id, number_of_layers, nodes_per_layer)
        allocate(bias(number_of_layers, nodes_per_layer))
        status = nf90_get_var(ncid, var_id, bias)
        if(status /= nf90_NoErr) call handle_err(status, "bias")

        call read_machine_learning_model_netcdf_real_2D_parameters(ncid, "weights", var_id, number_of_layers, nodes_per_layer)
        allocate(weights(number_of_layers, nodes_per_layer))
        status = nf90_get_var(ncid, var_id, weights)
        if(status /= nf90_NoErr) call handle_err(status, "weights")

        call read_machine_learning_model_netcdf_real_1D_parameters(ncid, "minimum_of_features", var_id, number_of_inputs)
        allocate(minimum_of_features(number_of_inputs))
        status = nf90_get_var(ncid, var_id, minimum_of_features)
        if(status /= nf90_NoErr) call handle_err(status, "minimum_of_features")

        call read_machine_learning_model_netcdf_real_1D_parameters(ncid, "maximum_of_features", var_id, number_of_inputs)
        allocate(maximum_of_features(number_of_inputs))
        status = nf90_get_var(ncid, var_id, maximum_of_features)
        if(status /= nf90_NoErr) call handle_err(status, "maximum_of_features")

        call read_machine_learning_model_netcdf_integer_1D_parameters(ncid, "activation_functions_ID", var_id, number_of_layers)
        allocate(activation_functions(number_of_layers))
        status = nf90_get_var(ncid, var_id, activation_functions)
        if(status /= nf90_NoErr) call handle_err(status, "activation_functions_ID")

        call read_machine_learning_model_netcdf_integer_1D_parameters(ncid, "number_of_nodes", var_id, number_of_layers)
        allocate(this%number_of_nodes(number_of_layers))
        status = nf90_get_var(ncid, var_id, this%number_of_nodes)
        if(status /= nf90_NoErr) call handle_err(status, "number_of_nodes")

        reshaped_weights = reshape(weights, (/this%number_of_nodes(2), this%number_of_nodes(3),4/))
        reshaped_weights_1st = reshaped_weights(1:this%number_of_nodes(2),1:this%number_of_nodes(1),1) !(1:90,1:6,1)
        reshaped_weights_2nd = reshaped_weights(:,:,2)
        reshaped_weights_3rd = reshaped_weights(:,:,3)
        reshaped_weights_4th = reshape(reshaped_weights(:,1,4),(/1, size(reshaped_weights(:,1,4))/))

        this%model_parameters(1)%link_weights= reshaped_weights_1st
        this%model_parameters(2)%link_weights= reshaped_weights_2nd
        this%model_parameters(3)%link_weights= reshaped_weights_3rd
        this%model_parameters(4)%link_weights= reshaped_weights_4th

        do node_index=1, (size(this%number_of_nodes)-1)
            this%model_parameters(node_index)%link_bias= reshape(bias(:, node_index), (/size(bias(:, node_index)),1/))
            this%model_parameters(node_index)%link_activation_functions = activation_functions(node_index)
        end do

        this%minimum_of_features_reshaped = (/minimum_of_features(1), minimum_of_features(6), minimum_of_features(3),&
        minimum_of_features(2), minimum_of_features(4), minimum_of_features(5)/)

        this%maximum_of_features_reshaped = (/maximum_of_features(1), maximum_of_features(6), maximum_of_features(3),&
        maximum_of_features(2), maximum_of_features(4), maximum_of_features(5)/)

        this%minimum_release = minimum_of_features(1)! minimum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!
        this%maximum_release = maximum_of_features(1)! maximum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!

        this%nn_inputs%previous_time_release = 0.0
        this%nn_inputs%current_storage = 0.0
        this%nn_inputs%inflow = 0.0
        this%nn_inputs%x_day_of_year = 0.0
        this%nn_inputs%y_day_of_year = 0.0

        ! Deallocate local subroutine arrays
        if(allocated(bias)) deallocate(bias)
        if(allocated(weights)) deallocate(weights)
        if(allocated(minimum_of_features)) deallocate(minimum_of_features)
        if(allocated(maximum_of_features)) deallocate(maximum_of_features)
        if(allocated(activation_functions)) deallocate(activation_functions)
        if(allocated(reshaped_weights)) deallocate(reshaped_weights)
        if(allocated(reshaped_weights_1st)) deallocate(reshaped_weights_1st)
        if(allocated(reshaped_weights_2nd)) deallocate(reshaped_weights_2nd)
        if(allocated(reshaped_weights_3rd)) deallocate(reshaped_weights_3rd)
        if(allocated(reshaped_weights_4th)) deallocate(reshaped_weights_4th)

    end subroutine machine_learning_model_init


    !Machine Learning Model Destructor
    subroutine machine_learning_model_destroy(this)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_model_destroy


    ! This subroutine is called from a machine learning reservoir's release function and performs
    ! matrix calculations to simulate reservoir release from a saved/trained ML model!
    subroutine run_machine_learning_model(model_data, inflow, previous_time_release, current_storage, &
        x_day_of_year, y_day_of_year, release)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: model_data

		real, intent (in)   ::  inflow
		real, intent (in)   ::  previous_time_release
		real, intent (in)   ::  current_storage
		real, intent (in)   ::  x_day_of_year
		real, intent (in)   ::  y_day_of_year
		real, intent (out)  ::  release

        integer :: next_layer_nodes, current_layer_nodes, layer_index
        real :: initial_matrix_sum

        real, allocatable, dimension(:) :: input_vector
        real, allocatable, dimension(:,:) :: reshaped_input_vector

        model_data%nn_inputs%previous_time_release = previous_time_release
        model_data%nn_inputs%current_storage = current_storage
        model_data%nn_inputs%inflow = inflow
        model_data%nn_inputs%x_day_of_year = x_day_of_year
        model_data%nn_inputs%y_day_of_year = y_day_of_year

        model_data%normalized_nn_inputs%previous_time_release = (model_data%nn_inputs%previous_time_release - model_data%minimum_of_features_reshaped(1))&
        /(model_data%maximum_of_features_reshaped(1) - model_data%minimum_of_features_reshaped(1))

        model_data%normalized_nn_inputs%current_storage = (model_data%nn_inputs%current_storage - model_data%minimum_of_features_reshaped(2))&
        /(model_data%maximum_of_features_reshaped(2) - model_data%minimum_of_features_reshaped(2))

        model_data%normalized_nn_inputs%inflow = (model_data%nn_inputs%inflow - model_data%minimum_of_features_reshaped(3))&
        /(model_data%maximum_of_features_reshaped(3) - model_data%minimum_of_features_reshaped(3))

        model_data%normalized_nn_inputs%x_day_of_year = model_data%nn_inputs%x_day_of_year ! this variable is already
        model_data%normalized_nn_inputs%y_day_of_year = model_data%nn_inputs%y_day_of_year

        ! Here input combination is assigned!
        input_vector = (/model_data%normalized_nn_inputs%previous_time_release, model_data%normalized_nn_inputs%x_day_of_year,&
            model_data%normalized_nn_inputs%y_day_of_year, model_data%normalized_nn_inputs%current_storage, &
            model_data%normalized_nn_inputs%inflow/)

        reshaped_input_vector = reshape(input_vector, (/ size(input_vector),1/))

        do layer_index=1, (size(model_data%number_of_nodes)-1) ! this represents in which layer we stand (i.e., hidden layers+output layer)
            model_data%nn_sum_matrix(:,:) = 0.0
            model_data%reshaped_nn_sum_matrix(:) = 0.0
            model_data%nn_sum_matrix_activated(:)= 0.0
            do next_layer_nodes = 1, model_data%number_of_nodes(layer_index+1)! nodes;90
                initial_matrix_sum = 0.0
                do current_layer_nodes= 1, model_data%number_of_nodes(layer_index)
                    if (layer_index==1) then
                        initial_matrix_sum = initial_matrix_sum + model_data%model_parameters(layer_index)%link_weights(next_layer_nodes, current_layer_nodes) * reshaped_input_vector(current_layer_nodes, 1) !reshaped_nn_inputs_local(j,1)
                        model_data%nn_sum_matrix(next_layer_nodes, 1) = initial_matrix_sum
                        model_data%nn_sum_matrix(next_layer_nodes, 1) = model_data%nn_sum_matrix(next_layer_nodes, 1) + model_data%model_parameters(layer_index)%link_bias(next_layer_nodes, 1)
                        model_data%reshaped_nn_sum_matrix(next_layer_nodes) = model_data%nn_sum_matrix(next_layer_nodes,1)
                        if (model_data%model_parameters(layer_index)%link_activation_functions(1)==0) then ! The focus will probably be on
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = sigmoid(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        else if (model_data%model_parameters(layer_index)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = relu(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        else
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = tanh(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        end if
                    else
                        initial_matrix_sum = initial_matrix_sum + model_data%model_parameters(layer_index)%link_weights(next_layer_nodes, current_layer_nodes) * model_data%reshaped_nn_sum_matrix_activated(current_layer_nodes, 1)
                        model_data%nn_sum_matrix(next_layer_nodes, 1) = initial_matrix_sum
                        model_data%nn_sum_matrix(next_layer_nodes, 1) = model_data%nn_sum_matrix(next_layer_nodes, 1) + model_data%model_parameters(layer_index)%link_bias(next_layer_nodes, 1)
                        model_data%reshaped_nn_sum_matrix(next_layer_nodes) = model_data%nn_sum_matrix(next_layer_nodes, 1)
                        if (model_data%model_parameters(layer_index)%link_activation_functions(1)==0) then
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = sigmoid(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        else if (model_data%model_parameters(layer_index)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = relu(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        else
                            model_data%nn_sum_matrix_activated(next_layer_nodes) = tanh(model_data%reshaped_nn_sum_matrix(next_layer_nodes))
                        end if
                    end if

                end do
            end do
            model_data%reshaped_nn_sum_matrix_activated = reshape(model_data%nn_sum_matrix_activated, (/ size(model_data%nn_sum_matrix_activated),1/))

        end do

        model_data%normalized_simulated_release = model_data%nn_sum_matrix_activated(1)

        ! Simulated release should be denormalized
		release = model_data%normalized_simulated_release * (model_data%maximum_release - model_data%minimum_release) + model_data%minimum_release ! first array refers to "Release" values; we could instead introduce it as

    end subroutine run_machine_learning_model


    ! We have just considered "sigmoid" and "relu" activation functions; later we can consider other activation functions
    real function sigmoid(z)
        real :: z, a
        a = 1.0/(1.0+ exp(-z))
        sigmoid = a
    end function sigmoid

    real function relu (z)
        real :: z, a
        if (z > 0.0) then
            a = z
        else
            a = 0.0
        end if
        relu = a
    end function relu

end module module_machine_learning_model

