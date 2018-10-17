
module module_machine_learning_model

    !use module_reservoir_base

    use netcdf
    implicit none


    type :: machine_learning_model_container
        class (machine_learning_model_struct), pointer :: ptr
    end type

    class (machine_learning_model_container), allocatable, dimension(:) :: machine_learning_models
    integer, allocatable, dimension(:) :: machine_learning_model_ids

    integer :: number_of_unique_models


    type NN_reservoir_in_out
        REAL :: previous_release ! We will probably need to have a dimension of "DIMENSION(NLAKES)"!
        REAL :: average_storage_over_past_time_intervals !
        REAL :: average_inflow_over_past_time_intervals  ! which corresponds to "QLAKEI": lake inflow (cms) or "qi1"
        REAL :: current_storage !
        REAL :: X_DOY ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
        REAL :: Y_DOY ! day of the year! some lines of the code need to be added for this

    end type NN_reservoir_in_out


    type NN_node
        real, allocatable, dimension (:,:) :: link_weights
        real, allocatable, dimension (:,:) :: link_bias
        integer, dimension (10) :: link_activation_functions ! FIXME, I had a hard time setting this into an allocatable dimension; so I had to fix it to a constant maximum number of layer = 10

    end type NN_node



    type :: machine_learning_model_struct

        integer :: model_number

        REAL :: X_DOY ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
        REAL :: Y_DOY ! day of the year! some lines of the code need to be added for this

        real, allocatable, dimension (:,:)   :: weights ! for each layer, there is a two-dimension matrix
        real, allocatable, dimension (:,:,:) :: reshaped_weights !
        real, allocatable, dimension (:,:)   :: reshaped_weights_1st
        real, allocatable, dimension (:,:)   :: reshaped_weights_2nd
        real, allocatable, dimension (:,:)   :: reshaped_weights_3rd
        real, allocatable, dimension (:,:)   :: reshaped_weights_4th

        real, allocatable, dimension (:,:)   :: bias ! Bias has one dimension less than that for the weights!

        integer, allocatable, dimension (:)  :: activation_functions ! is just a function of layer
        integer, allocatable, dimension (:)  :: number_of_nodes ! is just a function of layer; number of computational nodes (i.e., neurons); first element represents the number of inputs to the model
        real, allocatable, dimension (:)     :: minimum_of_features ! is a function of the size of the inputs
        real, allocatable, dimension (:)     :: maximum_of_features	! is a function of the size of the inputs

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

        integer, intent(in) :: model_number

        integer :: ncid, status, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId

        integer :: Nlayers                     ! number of model layers (this is actually the number of layers + 1, to help us for matrix dimensions)
        integer :: Ninputs                     ! total number of inputs, note that we might not be using all as the inputs to the ML model!
        integer :: nodes   ! # of neurons which is a function of layers and model number

        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs

        integer :: i, j, k

        ! If model_number is 1, then open ACF basin NetCDF. Else if model_number is 2, then open Upper Colorado basin NetCDF

		if (model_number == 1) then
			status = nf90_open(path = "ML_MODEL_PARM_ACF.nc", mode = nf90_nowrite, ncid = ncid)               ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status)


		else if (model_number == 2) then
			status = nf90_open(path = "ML_MODEL_PARM_CH.nc", mode = nf90_nowrite, ncid = ncid)                ! open existing NetCDF dataset
			if (status /= nf90_noerr) call handle_err(status)

		end if

        this%model_number = model_number


        !status = nf90_open(path = "17all_res_acf_all_relu_nc.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !if (status /= nf90_noerr) call handle_err(status)


        status = nf90_inq_varid(ncid, "bias", biasVarId)

        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "weights", weightsVarId)

        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "minimum_of_features", minimum_of_featuresVarId)


        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "maximum_of_features", maximum_of_featuresVarId)


        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "activation_functions_ID", activation_functionsVarId)


        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "number_of_nodes", number_of_nodesVarId)


        if(status /= nf90_NoErr) call handle_err(status)


        ! How big is the netCDF variable, that is, what are the lengths of
        !   its constituent dimensions?
        status = nf90_inquire_variable(ncid, biasVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = nodes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(this%bias(Nlayers, nodes))




        status = nf90_inquire_variable(ncid, weightsVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = nodes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(this%weights(Nlayers, nodes))


        status = nf90_inquire_variable(ncid, minimum_of_featuresVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(this%minimum_of_features(Ninputs))


        status = nf90_inquire_variable(ncid, maximum_of_featuresVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(this%maximum_of_features(Ninputs))



        status = nf90_inquire_variable(ncid, activation_functionsVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(this%activation_functions(Nlayers))


        status = nf90_inquire_variable(ncid, number_of_nodesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(this%number_of_nodes(Nlayers))






        status = nf90_get_var(ncid, biasVarId, this%bias)


        if (status /= nf90_noerr) call handle_err(status)



        status = nf90_get_var(ncid, weightsVarId, this%weights)

        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_get_var(ncid, minimum_of_featuresVarId, this%minimum_of_features)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, maximum_of_featuresVarId, this%maximum_of_features)

        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_get_var(ncid, activation_functionsVarId, this%activation_functions)


        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, number_of_nodesVarId, this%number_of_nodes)


        if (status /= nf90_noerr) call handle_err(status)




        this%reshaped_weights = reshape(this%weights, (/this%number_of_nodes(2), this%number_of_nodes(3),4/))

        this%reshaped_weights_1st = this%reshaped_weights(1:this%number_of_nodes(2),1:this%number_of_nodes(1),1) !(1:90,1:6,1)

        this%reshaped_weights_2nd = this%reshaped_weights(:,:,2)

        this%reshaped_weights_3rd = this%reshaped_weights(:,:,3)

        this%reshaped_weights_4th = reshape(this%reshaped_weights(:,1,4),(/1, size(this%reshaped_weights(:,1,4))/))


        do k=1, (size(this%number_of_nodes)-1)
            this%model_parameters(k)%link_bias= reshape(this%bias(:,k), (/size(this%bias(:,k)),1/))
        end do


        this%model_parameters(1)%link_weights= this%reshaped_weights_1st

        this%model_parameters(2)%link_weights= this%reshaped_weights_2nd

        this%model_parameters(3)%link_weights= this%reshaped_weights_3rd

        this%model_parameters(4)%link_weights= this%reshaped_weights_4th

        do k=1, (size(this%number_of_nodes)-1)
            this%model_parameters(k)%link_activation_functions = this%activation_functions(k)
        end do


        !Linear interpolation to calculate "previous storage" based on the "previous pool elevation" using a linear interpolation on "acap" table!
        !FIXME Nels suggested to use "binary search" for this purpose;
        !WARNING: note if this is the case, we will need to train the model based on these new way of calculation of storage from acap files!
        !This code for linear interpolation needs some modifications as well!


        this%nn_inputs%previous_release = 0.0
        this%nn_inputs%average_storage_over_past_time_intervals = 0.0
        this%nn_inputs%average_inflow_over_past_time_intervals = 0.0
        this%nn_inputs%X_DOY = 0.0
        this%nn_inputs%Y_DOY = 0.0



        ! this has been intentionally deactivated;  Index([u'Release', u'Elevation', u'Inflow', u'DOX', u'DOY', u'Storage'], dtype='object') coming from ACF NetCDF file
        this%minimum_of_features_final = (/this%minimum_of_features(1), this%minimum_of_features(6), this%minimum_of_features(3),&
         this%minimum_of_features(2), this%minimum_of_features(4), this%minimum_of_features(5)/)

        this%maximum_of_features_final = (/this%maximum_of_features(1), this%maximum_of_features(6), this%maximum_of_features(3),&
         this%maximum_of_features(2), this%maximum_of_features(4), this%maximum_of_features(5)/)



        !this%normalized_nn_inputs%previous_release = (this%nn_inputs%previous_release - this%minimum_of_features_final(1))&
        !/(this%maximum_of_features_final(1) - this%minimum_of_features_final(1))

        !this%normalized_nn_inputs%previous_storage = (this%nn_inputs%previous_storage - this%minimum_of_features_final(2))&
        !/(this%maximum_of_features_final(2) - this%minimum_of_features_final(2))

        !this%normalized_nn_inputs%inflow = (this%nn_inputs%inflow - this%minimum_of_features_final(3))&
        !/(this%maximum_of_features_final(3) - this%minimum_of_features_final(3))


        !this%normalized_nn_inputs%X_DOY = this%nn_inputs%X_DOY ! this variable is already
        !this%normalized_nn_inputs%Y_DOY = this%nn_inputs%Y_DOY



        this%y_min = this%minimum_of_features(1)! minimum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!
        this%y_max = this%maximum_of_features(1)! maximum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!



        ! Should we deallocate and should I just assign these as local instead????
        ! Deallocation of memory

        deallocate(this%bias)
        deallocate(this%weights)
        deallocate(this%minimum_of_features)
        deallocate(this%maximum_of_features)
        deallocate(this%activation_functions)
        !deallocate(number_of_nodes)



    end subroutine machine_learning_model_init




    !Machine Learning Model Destructor
    subroutine machine_learning_model_destroy(this)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_model_destroy






    ! This subroutine performs matrix calculations to simulate reservoir release from a saved/trained ML model!
    subroutine run_machine_learning_model(model_data, average_inflow_over_past_time_intervals, release, previous_release, average_storage_over_past_time_intervals, X_DOY, Y_DOY)
        implicit none
        !class(machine_learning_model_struct), intent(inout) :: this
        class(machine_learning_model_struct), intent(inout) :: model_data
        !real ::

        !type (NN_node), dimension (10), intent (IN) :: model_parameters_local

		real, intent (in)   ::  average_inflow_over_past_time_intervals
		real, intent (out)  ::  release
		real, intent (in)   ::  previous_release
		real, intent (in)   ::  average_storage_over_past_time_intervals
		real, intent (in)   ::  X_DOY
		real, intent (in)   ::  Y_DOY



        real, allocatable, dimension(:) :: input_vector_local
        real, allocatable, dimension(:,:) :: reshaped_input_vector_local
        !real, intent(OUT) :: ml_simulated_release_local

        !type (NN_reservoir_in_out)  :: normalized_nn_inputs_local

        integer :: i, j, k
        real tmp


        model_data%nn_inputs%previous_release = previous_release
        model_data%nn_inputs%average_storage_over_past_time_intervals = average_storage_over_past_time_intervals
        model_data%nn_inputs%average_inflow_over_past_time_intervals = average_inflow_over_past_time_intervals
        model_data%nn_inputs%X_DOY = X_DOY
        model_data%nn_inputs%Y_DOY = Y_DOY


	model_data%normalized_nn_inputs%previous_release = (model_data%nn_inputs%previous_release - model_data%minimum_of_features_final(1))&
        /(model_data%maximum_of_features_final(1) - model_data%minimum_of_features_final(1))

        model_data%normalized_nn_inputs%average_storage_over_past_time_intervals = (model_data%nn_inputs%average_storage_over_past_time_intervals - model_data%minimum_of_features_final(2))&
        /(model_data%maximum_of_features_final(2) - model_data%minimum_of_features_final(2))

        model_data%normalized_nn_inputs%average_inflow_over_past_time_intervals = (model_data%nn_inputs%average_inflow_over_past_time_intervals - model_data%minimum_of_features_final(3))&
        /(model_data%maximum_of_features_final(3) - model_data%minimum_of_features_final(3))


        model_data%normalized_nn_inputs%X_DOY = model_data%nn_inputs%X_DOY ! this variable is already
        model_data%normalized_nn_inputs%Y_DOY = model_data%nn_inputs%Y_DOY


        ! Here input combination is assigned!


        !model_data%input_vector = (/normalized_nn_inputs_local%previous_release, normalized_nn_inputs_local%X_DOY,&
        !    normalized_nn_inputs_local%Y_DOY, normalized_nn_inputs_local%previous_storage, &
        !    normalized_nn_inputs_local%inflow/)

        model_data%input_vector = (/model_data%normalized_nn_inputs%previous_release, model_data%normalized_nn_inputs%X_DOY,&
            model_data%normalized_nn_inputs%Y_DOY, model_data%normalized_nn_inputs%average_storage_over_past_time_intervals, &
            model_data%normalized_nn_inputs%average_inflow_over_past_time_intervals/)


        reshaped_input_vector_local = reshape(model_data%input_vector, (/ size(model_data%input_vector),1/))


        do k=1, (size(model_data%number_of_nodes)-1) ! this represents in which layer we stand (i.e., hidden layers+output layer)
            model_data%nn_sum_matrix(:,:) = 0.0
            model_data%reshaped_nn_sum_matrix(:) = 0.0
            model_data%nn_sum_matrix_activated(:)= 0.0
            do i = 1, model_data%number_of_nodes(k+1)! nodes;90
                tmp = 0.0
                do j= 1, model_data%number_of_nodes(k)
                    if (k==1) then
                        tmp = tmp + model_data%model_parameters(k)%link_weights(i,j) * reshaped_input_vector_local(j,1) !reshaped_nn_inputs_local(j,1)
                        model_data%nn_sum_matrix(i,1) = tmp
                        model_data%nn_sum_matrix(i,1) = model_data%nn_sum_matrix(i,1) + model_data%model_parameters(k)%link_bias(i,1)
                        model_data%reshaped_nn_sum_matrix(i) = model_data%nn_sum_matrix(i,1)
                        if (model_data%model_parameters(k)%link_activation_functions(1)==0) then ! The focus will probably be on
                            model_data%nn_sum_matrix_activated(i) = sigmoid(model_data%reshaped_nn_sum_matrix(i))
                        else if (model_data%model_parameters(k)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(i) = relu(model_data%reshaped_nn_sum_matrix(i))
                        else
                            model_data%nn_sum_matrix_activated(i) = tanh(model_data%reshaped_nn_sum_matrix(i))
                        end if
                    else
                        tmp = tmp + model_data%model_parameters(k)%link_weights(i,j) * model_data%reshaped_nn_sum_matrix_activated(j,1)
                        model_data%nn_sum_matrix(i,1) = tmp
                        model_data%nn_sum_matrix(i,1) = model_data%nn_sum_matrix(i,1) + model_data%model_parameters(k)%link_bias(i,1)
                        model_data%reshaped_nn_sum_matrix(i) = model_data%nn_sum_matrix(i,1)
                        if (model_data%model_parameters(k)%link_activation_functions(1)==0) then
                            model_data%nn_sum_matrix_activated(i) = sigmoid(model_data%reshaped_nn_sum_matrix(i))
                        else if (model_data%model_parameters(k)%link_activation_functions(1)==1) then
                            model_data%nn_sum_matrix_activated(i) = relu(model_data%reshaped_nn_sum_matrix(i))
                        else
                            model_data%nn_sum_matrix_activated(i) = tanh(model_data%reshaped_nn_sum_matrix(i))
                        end if
                    end if

                end do
            end do
            model_data%reshaped_nn_sum_matrix_activated = reshape(model_data%nn_sum_matrix_activated, (/ size(model_data%nn_sum_matrix_activated),1/))
            print *, "k", k

        end do

        model_data%normalized_simulated_release = model_data%nn_sum_matrix_activated(1)



        ! Simulated release should be denormalized
        !ml_simulated_release_local = model_data%normalized_simulated_release * (model_data%y_max - model_data%y_min) + model_data%y_min ! first array refers to "Release" values; we could instead introduce it as
        !model_data%ml_simulated_release = ml_simulated_release_local

		release = model_data%normalized_simulated_release * (model_data%y_max - model_data%y_min) + model_data%y_min ! first array refers to "Release" values; we could instead introduce it as


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FIXME The corrector terms need to be added here (we already have them in the Python)

        !Calculate the reservoir storage at the current time (i.e., the storage that corresponds to the simulated release); this part will need revision for the operational version

        !FIXME Copied from Python


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!End of the corrector terms

        ! !qo1 = revised_simulated_release ! the revised version of the simulated release is sent back to the same variable within "LEVELPOOL" subroutine!




        !!##########################################################################################################################################



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

