
module module_machine_learning_model

    use netcdf
    use module_reservoir_utilities
    !use module_machine_learning_utilities
    implicit none


    type :: machine_learning_model_container
        class (machine_learning_model_struct), pointer :: ptr
    end type

    class (machine_learning_model_container), allocatable, dimension(:) :: machine_learning_models
    integer, allocatable, dimension(:) :: machine_learning_model_ids

    integer :: number_of_unique_ml_models


    type NN_reservoir_in_out
        REAL :: previous_time_release ! We will probably need to have a dimension of "DIMENSION(NLAKES)"!
        REAL :: inflow  ! which corresponds to "QLAKEI": lake inflow (cms) or "qi1"
        REAL :: current_storage !
        REAL :: x_day_of_year ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
        REAL :: y_day_of_year ! day of the year! some lines of the code need to be added for this

    end type NN_reservoir_in_out


    type NN_node
        real, allocatable, dimension (:,:) :: link_weights
        real, allocatable, dimension (:,:) :: link_bias
        integer, dimension (10) :: link_activation_functions ! FIXME, I had a hard time setting this into an allocatable dimension; so I had to fix it to a constant maximum number of layer = 10

    end type NN_node


    type :: machine_learning_model_struct

        integer :: model_number

        REAL :: x_day_of_year ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
        REAL :: y_day_of_year ! day of the year! some lines of the code need to be added for this

        !real, allocatable, dimension (:,:)   :: weights ! for each layer, there is a two-dimension matrix
        !real, allocatable, dimension (:,:,:) :: reshaped_weights !
        !real, allocatable, dimension (:,:)   :: reshaped_weights_1st
        !real, allocatable, dimension (:,:)   :: reshaped_weights_2nd
        !real, allocatable, dimension (:,:)   :: reshaped_weights_3rd
        !real, allocatable, dimension (:,:)   :: reshaped_weights_4th

        !real, allocatable, dimension (:,:)   :: bias ! Bias has one dimension less than that for the weights!

        !integer, allocatable, dimension (:)  :: activation_functions ! is just a function of layer
        integer, allocatable, dimension (:)  :: number_of_nodes ! is just a function of layer; number of computational nodes (i.e., neurons); first element represents the number of inputs to the model
        !real, allocatable, dimension (:)     :: minimum_of_features ! is a function of the size of the inputs
        !real, allocatable, dimension (:)     :: maximum_of_features	! is a function of the size of the inputs

        real, allocatable, dimension (:)     :: minimum_of_features_final
        real, allocatable, dimension (:)     :: maximum_of_features_final


        ! Parameter declaration for ml_res_sim_release subroutine
        real :: inflow_current_time       ! inflow at the current timestep (cms); "qi1" at the "LEVELPOOL" subroutine
        real :: revised_simulated_release     ! outflow at current timestep; "qo1" at the "LEVELPOOL" subroutine
        real :: normalized_simulated_release, ml_simulated_release, revised_release, desired_release, tmp, tmp1


        real, dimension  (1000,1) :: nn_sum_matrix ! The sum of matrix calculation(Wx+b) before applying transfer function!
        real, dimension  (1000,1) :: reshaped_nn_sum_matrix_activated

        real :: y_max, y_min
        real :: y
        real, dimension (1000) :: reshaped_nn_sum_matrix ! a reshaped version of the "nn_sum_matrix"
        real, dimension (1000) :: nn_sum_matrix_activated ! the output of the NN by applying the transfer function on "nn_sum_matrix"


        type (NN_node), dimension (10)  :: model_parameters

        type (NN_reservoir_in_out)  :: nn_inputs ! the dimension should apparently represent the number of inputs
        type (NN_reservoir_in_out)  :: normalized_nn_inputs
        type (NN_reservoir_in_out) :: reshaped_nn_inputs

        real, allocatable, dimension(:) :: input_vector
        real, allocatable, dimension(:,:) :: reshaped_input_vector


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


        real, allocatable, dimension (:,:)   :: bias ! Bias has one dimension less than that for the weights!
        real, allocatable, dimension (:,:)   :: weights ! for each layer, there is a two-dimension matrix
        real, allocatable, dimension (:)     :: minimum_of_features ! is a function of the size of the inputs
        real, allocatable, dimension (:)     :: maximum_of_features	! is a function of the size of the inputs
        integer, allocatable, dimension (:)  :: activation_functions ! is just a function of layer

        real, allocatable, dimension (:,:,:) :: reshaped_weights !
        real, allocatable, dimension (:,:)   :: reshaped_weights_1st
        real, allocatable, dimension (:,:)   :: reshaped_weights_2nd
        real, allocatable, dimension (:,:)   :: reshaped_weights_3rd
        real, allocatable, dimension (:,:)   :: reshaped_weights_4th



        integer, intent(in) :: model_number

        integer :: ncid, var_id, status, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId

        integer :: Nlayers                     ! number of model layers (this is actually the number of layers + 1, to help us for matrix dimensions)
        integer :: Ninputs                     ! total number of inputs, note that we might not be using all as the inputs to the ML model!
        integer :: number_of_inputs
        integer :: nodes   ! # of neurons which is a function of layers and model number
        integer :: number_of_layers

        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs

        integer :: i, j, k
print *, '1.19'
        ! If model_number is 1, then open ACF basin NetCDF. Else if model_number is 2, then open Upper Colorado basin NetCDF

		if (model_number == 1) then
			status = nf90_open(path = "ML_MODEL_PARM_ACF.nc", mode = nf90_nowrite, ncid = ncid)               ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status, "ML_MODEL_PARM_ACF.nc")


		else if (model_number == 2) then
			status = nf90_open(path = "ML_MODEL_PARM_CH.nc", mode = nf90_nowrite, ncid = ncid)                ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status, "ML_MODEL_PARM_CH.nc")

		end if

print *, '2.0'
        this%model_number = model_number

print *, '2.1'

        call read_machine_learning_model_netcdf_real_2D_parameters(ncid, "bias", var_id, number_of_layers, nodes)
        allocate(bias(number_of_layers, nodes))
        status = nf90_get_var(ncid, var_id, bias)
        if(status /= nf90_NoErr) call handle_err(status, "bias")

print *, '2.2'
        call read_machine_learning_model_netcdf_real_2D_parameters(ncid, "weights", var_id, number_of_layers, nodes)
        allocate(weights(number_of_layers, nodes))
        status = nf90_get_var(ncid, var_id, weights)
        if(status /= nf90_NoErr) call handle_err(status, "weights")

print *, '2.3'
        call read_machine_learning_model_netcdf_real_1D_parameters(ncid, "minimum_of_features", var_id, number_of_inputs)
        allocate(minimum_of_features(number_of_inputs))
        status = nf90_get_var(ncid, var_id, minimum_of_features)
        if(status /= nf90_NoErr) call handle_err(status, "minimum_of_features")

print *, '2.4'
        call read_machine_learning_model_netcdf_real_1D_parameters(ncid, "maximum_of_features", var_id, number_of_inputs)
        allocate(maximum_of_features(number_of_inputs))
        status = nf90_get_var(ncid, var_id, maximum_of_features)
        if(status /= nf90_NoErr) call handle_err(status, "maximum_of_features")

print *, '2.5'
        call read_machine_learning_model_netcdf_integer_1D_parameters(ncid, "activation_functions_ID", var_id, number_of_layers)
        allocate(activation_functions(number_of_layers))
        status = nf90_get_var(ncid, var_id, activation_functions)
        if(status /= nf90_NoErr) call handle_err(status, "activation_functions_ID")

print *, '2.6'
        call read_machine_learning_model_netcdf_integer_1D_parameters(ncid, "number_of_nodes", var_id, number_of_layers)
        allocate(this%number_of_nodes(number_of_layers))
        status = nf90_get_var(ncid, var_id, this%number_of_nodes)
        if(status /= nf90_NoErr) call handle_err(status, "number_of_nodes")


print *, '2.7'

        !status = nf90_inq_varid(ncid, "bias", biasVarId)

        !if(status /= nf90_NoErr) call handle_err(status, )


        !status = nf90_inq_varid(ncid, "weights", weightsVarId)

        !if(status /= nf90_NoErr) call handle_err(status, )


        !status = nf90_inq_varid(ncid, "minimum_of_features", minimum_of_featuresVarId)


        !if(status /= nf90_NoErr) call handle_err(status, )


        !status = nf90_inq_varid(ncid, "maximum_of_features", maximum_of_featuresVarId)


        !if(status /= nf90_NoErr) call handle_err(status, )


        !status = nf90_inq_varid(ncid, "activation_functions_ID", activation_functionsVarId)


        !if(status /= nf90_NoErr) call handle_err(status, )


        !status = nf90_inq_varid(ncid, "number_of_nodes", number_of_nodesVarId)


        !if(status /= nf90_NoErr) call handle_err(status, )



        ! How big is the netCDF variable, that is, what are the lengths of
        !   its constituent dimensions?
        !status = nf90_inquire_variable(ncid, biasVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )


       ! status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(2), len = nodes)
        !if(status /= nf90_NoErr) call handle_err(status, )


        !allocate(this%bias(Nlayers, nodes))
        !allocate(this%bias(nodes, Nlayers))


        !status = nf90_inquire_variable(ncid, weightsVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(2), len = nodes)
        !if(status /= nf90_NoErr) call handle_err(status, )


        !allocate(this%weights(Nlayers, nodes))

        !status = nf90_inquire_variable(ncid, minimum_of_featuresVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !allocate(this%minimum_of_features(Ninputs))


        !status = nf90_inquire_variable(ncid, maximum_of_featuresVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !allocate(this%maximum_of_features(Ninputs))

        !status = nf90_inquire_variable(ncid, activation_functionsVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !allocate(this%activation_functions(Nlayers))


        !status = nf90_inquire_variable(ncid, number_of_nodesVarId, dimids = dimIDs)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        !if(status /= nf90_NoErr) call handle_err(status, )

        !allocate(this%number_of_nodes(Nlayers))



        !status = nf90_get_var(ncid, biasVarId, this%bias)


        !if (status /= nf90_noerr) call handle_err(status, )


        !status = nf90_get_var(ncid, weightsVarId, this%weights)

        !if (status /= nf90_noerr) call handle_err(status, )


        !status = nf90_get_var(ncid, minimum_of_featuresVarId, this%minimum_of_features)

        !if (status /= nf90_noerr) call handle_err(status, )


        !status = nf90_get_var(ncid, maximum_of_featuresVarId, this%maximum_of_features)

        !if (status /= nf90_noerr) call handle_err(status, )


        !status = nf90_get_var(ncid, activation_functionsVarId, this%activation_functions)

        !if (status /= nf90_noerr) call handle_err(status, )


        !status = nf90_get_var(ncid, number_of_nodesVarId, this%number_of_nodes)

        !if (status /= nf90_noerr) call handle_err(status, )


print *, '2.8'
        reshaped_weights = reshape(weights, (/this%number_of_nodes(2), this%number_of_nodes(3),4/))

        reshaped_weights_1st = reshaped_weights(1:this%number_of_nodes(2),1:this%number_of_nodes(1),1) !(1:90,1:6,1)

        reshaped_weights_2nd = reshaped_weights(:,:,2)

        reshaped_weights_3rd = reshaped_weights(:,:,3)

        reshaped_weights_4th = reshape(reshaped_weights(:,1,4),(/1, size(reshaped_weights(:,1,4))/))

print *, '2.9'
        do k=1, (size(this%number_of_nodes)-1)
            this%model_parameters(k)%link_bias= reshape(bias(:,k), (/size(bias(:,k)),1/))
        end do


        this%model_parameters(1)%link_weights= reshaped_weights_1st

        this%model_parameters(2)%link_weights= reshaped_weights_2nd

        this%model_parameters(3)%link_weights= reshaped_weights_3rd

        this%model_parameters(4)%link_weights= reshaped_weights_4th

        do k=1, (size(this%number_of_nodes)-1)
            this%model_parameters(k)%link_activation_functions = activation_functions(k)
        end do


        this%nn_inputs%previous_time_release = 0.0
        this%nn_inputs%current_storage = 0.0
        this%nn_inputs%inflow = 0.0
        this%nn_inputs%x_day_of_year = 0.0
        this%nn_inputs%y_day_of_year = 0.0


        this%minimum_of_features_final = (/minimum_of_features(1), minimum_of_features(6), minimum_of_features(3),&
         minimum_of_features(2), minimum_of_features(4), minimum_of_features(5)/)

        this%maximum_of_features_final = (/maximum_of_features(1), maximum_of_features(6), maximum_of_features(3),&
         maximum_of_features(2), maximum_of_features(4), maximum_of_features(5)/)


        this%y_min = minimum_of_features(1)! minimum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!
        this%y_max = maximum_of_features(1)! maximum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!


print *, '3.0'

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



print *, '3.1'
    end subroutine machine_learning_model_init




    !Machine Learning Model Destructor
    subroutine machine_learning_model_destroy(this)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_model_destroy


    ! This subroutine performs matrix calculations to simulate reservoir release from a saved/trained ML model!
    subroutine run_machine_learning_model(model_data, inflow, previous_time_release, current_storage, x_day_of_year, y_day_of_year, release)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: model_data

		real, intent (in)   ::  inflow
		real, intent (out)  ::  release
		real, intent (in)   ::  previous_time_release
		real, intent (in)   ::  current_storage
		real, intent (in)   ::  x_day_of_year
		real, intent (in)   ::  y_day_of_year

        real, allocatable, dimension(:) :: input_vector_local
        real, allocatable, dimension(:,:) :: reshaped_input_vector_local

        !integer :: i, j, k
        integer :: i, j, layer_index
        real tmp

        model_data%nn_inputs%previous_time_release = previous_time_release
        model_data%nn_inputs%current_storage = current_storage
        model_data%nn_inputs%inflow = inflow
        model_data%nn_inputs%x_day_of_year = x_day_of_year
        model_data%nn_inputs%y_day_of_year = y_day_of_year


        model_data%normalized_nn_inputs%previous_time_release = (model_data%nn_inputs%previous_time_release - model_data%minimum_of_features_final(1))&
        /(model_data%maximum_of_features_final(1) - model_data%minimum_of_features_final(1))

        model_data%normalized_nn_inputs%current_storage = (model_data%nn_inputs%current_storage - model_data%minimum_of_features_final(2))&
        /(model_data%maximum_of_features_final(2) - model_data%minimum_of_features_final(2))

        model_data%normalized_nn_inputs%inflow = (model_data%nn_inputs%inflow - model_data%minimum_of_features_final(3))&
        /(model_data%maximum_of_features_final(3) - model_data%minimum_of_features_final(3))


        model_data%normalized_nn_inputs%x_day_of_year = model_data%nn_inputs%x_day_of_year ! this variable is already
        model_data%normalized_nn_inputs%y_day_of_year = model_data%nn_inputs%y_day_of_year


        ! Here input combination is assigned!
        model_data%input_vector = (/model_data%normalized_nn_inputs%previous_time_release, model_data%normalized_nn_inputs%x_day_of_year,&
            model_data%normalized_nn_inputs%y_day_of_year, model_data%normalized_nn_inputs%current_storage, &
            model_data%normalized_nn_inputs%inflow/)


        reshaped_input_vector_local = reshape(model_data%input_vector, (/ size(model_data%input_vector),1/))


        do layer_index=1, (size(model_data%number_of_nodes)-1) ! this represents in which layer we stand (i.e., hidden layers+output layer)
            model_data%nn_sum_matrix(:,:) = 0.0
            model_data%reshaped_nn_sum_matrix(:) = 0.0
            model_data%nn_sum_matrix_activated(:)= 0.0
            do i = 1, model_data%number_of_nodes(layer_index+1)! nodes;90
                tmp = 0.0
                do j= 1, model_data%number_of_nodes(layer_index)
                    if (layer_index==1) then
                        tmp = tmp + model_data%model_parameters(layer_index)%link_weights(i,j) * reshaped_input_vector_local(j,1) !reshaped_nn_inputs_local(j,1)
                        model_data%nn_sum_matrix(i,1) = tmp
                        model_data%nn_sum_matrix(i,1) = model_data%nn_sum_matrix(i,1) + model_data%model_parameters(layer_index)%link_bias(i,1)
                        model_data%reshaped_nn_sum_matrix(i) = model_data%nn_sum_matrix(i,1)
                        if (model_data%model_parameters(layer_index)%link_activation_functions(1)==0) then ! The focus will probably be on
                            model_data%nn_sum_matrix_activated(i) = sigmoid(model_data%reshaped_nn_sum_matrix(i))
                        else if (model_data%model_parameters(layer_index)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(i) = relu(model_data%reshaped_nn_sum_matrix(i))
                        else
                            model_data%nn_sum_matrix_activated(i) = tanh(model_data%reshaped_nn_sum_matrix(i))
                        end if
                    else
                        tmp = tmp + model_data%model_parameters(layer_index)%link_weights(i,j) * model_data%reshaped_nn_sum_matrix_activated(j,1)
                        model_data%nn_sum_matrix(i,1) = tmp
                        model_data%nn_sum_matrix(i,1) = model_data%nn_sum_matrix(i,1) + model_data%model_parameters(layer_index)%link_bias(i,1)
                        model_data%reshaped_nn_sum_matrix(i) = model_data%nn_sum_matrix(i,1)
                        if (model_data%model_parameters(layer_index)%link_activation_functions(1)==0) then
                            model_data%nn_sum_matrix_activated(i) = sigmoid(model_data%reshaped_nn_sum_matrix(i))
                        else if (model_data%model_parameters(layer_index)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(i) = relu(model_data%reshaped_nn_sum_matrix(i))
                        else
                            model_data%nn_sum_matrix_activated(i) = tanh(model_data%reshaped_nn_sum_matrix(i))
                        end if
                    end if

                end do
            end do
            model_data%reshaped_nn_sum_matrix_activated = reshape(model_data%nn_sum_matrix_activated, (/ size(model_data%nn_sum_matrix_activated),1/))

        end do

        model_data%normalized_simulated_release = model_data%nn_sum_matrix_activated(1)


        ! Simulated release should be denormalized
		release = model_data%normalized_simulated_release * (model_data%y_max - model_data%y_min) + model_data%y_min ! first array refers to "Release" values; we could instead introduce it as


    end subroutine run_machine_learning_model



    subroutine read_machine_learning_model_netcdf_integer_1D_parameters(ncid, netcdf_array_name, var_id, number_of_inputs)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_inputs
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_inputs)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_integer_1D_parameters



    subroutine read_machine_learning_model_netcdf_real_1D_parameters(ncid, netcdf_array_name, var_id, number_of_inputs)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_inputs
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_inputs)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_real_1D_parameters


    subroutine read_machine_learning_model_netcdf_real_2D_parameters(ncid, netcdf_array_name, var_id, number_of_layers, number_of_nodes)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_layers, number_of_nodes
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_layers)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_nodes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_real_2D_parameters





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

