module module_machine_learning_model

    !use module_reservoir_base
    implicit none


    type :: machine_learning_model_struct

        !integer :: lake_id      ! lake id or COMID


        REAL :: X_DOY ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
        REAL :: Y_DOY ! day of the year! some lines of the code need to be added for this

        integer :: Nlayers                     ! number of model layers (this is actually the number of layers + 1, to help us for matrix dimensions)
        integer :: Ninputs                     ! total number of inputs, note that we might not be using all as the inputs to the ML model!
        integer :: str_len ! it is supposed to be one of the dimensions for "input_names"; however, I had problem in reading/assigning "input_names"
        integer :: Nlakes !
        integer :: monthly_averages_rows !

        integer :: nodes   ! # of neurons which is a function of layers and model number
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


        real :: mmm
        real :: nnn
        real, allocatable, dimension(:)     :: y_linear_interp
        real :: slope



        ! Parameter declaration for ml_res_sim_release subroutine
        real :: inflow_previous_time      ! inflow at previous timestep (cms);  "qi0" at the "LEVELPOOL" subroutine
        real :: inflow_current_time       ! inflow at the current timestep (cms); "qi1" at the "LEVELPOOL" subroutine
        real :: revised_simulated_release     ! outflow at current timestep; "qo1" at the "LEVELPOOL" subroutine
        real :: normalized_simulated_release, ml_simulated_release, revised_release, desired_release, tmp, tmp1


        real, dimension  (1000,1) :: nn_sum_matrix ! The sum of matrix calculation(Wx+b) before applying transfer function!
        real, dimension  (1000,1) :: reshaped_nn_sum_matrix_activated

        real :: y_max, y_min
        real :: y
        real, dimension (1000) :: reshaped_nn_sum_matrix ! a reshaped version of the "nn_sum_matrix"
        real, dimension (1000) :: nn_sum_matrix_activated ! the output of the NN by applying the transfer function on "nn_sum_matrix"

        real, allocatable, dimension (:) :: softmax, linear


        type NN_node
            real, allocatable, dimension (:,:) :: link_weights
            real, allocatable, dimension (:,:) :: link_bias
            integer, dimension (10) :: link_activation_functions ! FIXME, I had a hard time setting this into an allocatable dimension; so I had to fix it to a constant maximum number of layer = 10

        end type NN_node

        type (NN_node), dimension (10)  :: model_parameters


        type NN_reservoir_in_out
            REAL :: previous_release ! We will probably need to have a dimension of "DIMENSION(NLAKES)"!
            REAL :: previous_storage !
            REAL :: inflow  ! which corresponds to "QLAKEI": lake inflow (cms) or "qi1"
            REAL :: pool_elevation !
            REAL :: previous_pool_elevation !
            REAL :: current_storage !
            REAL :: X_DOY ! day of the year; some lines of the code need to be added for this; could be added to "reservoir_init"
            REAL :: Y_DOY ! day of the year! some lines of the code need to be added for this

        end type NN_reservoir_in_out

        type (NN_reservoir_in_out)  :: nn_inputs ! the dimension should apparently represent the number of inputs
        type (NN_reservoir_in_out)  :: normalized_nn_inputs
        type (NN_reservoir_in_out) :: reshaped_nn_inputs

        real, allocatable, dimension(:) :: input_vector
        real, allocatable, dimension(:,:) :: reshaped_input_vector

        real :: minRelease
        real :: maxRelease
        real :: minStorage
        real :: maxStorage



    contains

        procedure :: init => machine_learning_model_init
        procedure :: destroy => machine_learning_model_destroy
        procedure :: run_model => run_machine_learning_model

    end type machine_learning_model_struct

contains


    !Machine Learning Model Constructor
    subroutine machine_learning_model_init(this, start_date, lake_number)
        use netcdf
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being initialized


        integer :: ncid, status, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId, minimum_storageVarId, maximum_storageVarId, lake_idVarId
        integer :: minimum_releaseVarId, maximum_releaseVarId
        integer :: long_term_releaseVarId

        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs

        integer :: ID_monthly_averagesVarId, release_monthly_averagesVarId, elevation_monthly_averagesVarId
        integer :: inflow_monthly_averagesVarId, storage_monthly_averagesVarId

        status = nf90_open(path = "17all_res_acf_all_relu_nc.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status)



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


        allocate(bias(Nlayers, nodes))




        status = nf90_inquire_variable(ncid, weightsVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = nodes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(weights(Nlayers, nodes))


        status = nf90_inquire_variable(ncid, minimum_of_featuresVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(minimum_of_features(Ninputs))


        status = nf90_inquire_variable(ncid, maximum_of_featuresVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Ninputs)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(maximum_of_features(Ninputs))



        status = nf90_inquire_variable(ncid, activation_functionsVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(activation_functions(Nlayers))


        status = nf90_inquire_variable(ncid, number_of_nodesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlayers)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(number_of_nodes(Nlayers))


        status = nf90_get_var(ncid, biasVarId, bias)


        if (status /= nf90_noerr) call handle_err(status)




        status = nf90_get_var(ncid, weightsVarId, weights)

        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_get_var(ncid, minimum_of_featuresVarId, minimum_of_features)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, maximum_of_featuresVarId, maximum_of_features)

        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_get_var(ncid, activation_functionsVarId, activation_functions)


        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, number_of_nodesVarId, number_of_nodes)


        if (status /= nf90_noerr) call handle_err(status)




        reshaped_weights = reshape(weights, (/number_of_nodes(2),number_of_nodes(3),4/))

        reshaped_weights_1st = reshaped_weights(1:number_of_nodes(2),1:number_of_nodes(1),1) !(1:90,1:6,1)

        reshaped_weights_2nd = reshaped_weights(:,:,2)

        reshaped_weights_3rd = reshaped_weights(:,:,3)

        reshaped_weights_4th = reshape(reshaped_weights(:,1,4),(/1, size(reshaped_weights(:,1,4))/))


        do k=1, (size(number_of_nodes)-1)
            model_parameters(k)%link_bias= reshape(bias(:,k), (/size(bias(:,k)),1/))
        end do


        model_parameters(1)%link_weights= reshaped_weights_1st

        model_parameters(2)%link_weights= reshaped_weights_2nd

        model_parameters(3)%link_weights= reshaped_weights_3rd

        model_parameters(4)%link_weights= reshaped_weights_4th

        do k=1, (size(number_of_nodes)-1)
            model_parameters(k)%link_activation_functions = activation_functions(k)
        end do


        !Linear interpolation to calculate "previous storage" based on the "previous pool elevation" using a linear interpolation on "acap" table!
        !FIXME Nels suggested to use "binary search" for this purpose;
        !WARNING: note if this is the case, we will need to train the model based on these new way of calculation of storage from acap files!
        !This code for linear interpolation needs some modifications as well!

        nn_inputs%previous_pool_elevation = 184 !


        nn_inputs%previous_release = 187.78992  !4.090795709653281126e-02
        nn_inputs%previous_storage = 9.700250e+08  !2.938184733232386270e-01
        nn_inputs%inflow = 192.46272  !3.645985341167530536e-02
        nn_inputs%X_DOY = 4.93544852e-01
        nn_inputs%Y_DOY = 4.16706726e-05


        ! this has been intentionally deactivated;  Index([u'Release', u'Elevation', u'Inflow', u'DOX', u'DOY', u'Storage'], dtype='object') coming from ACF NetCDF file
        minimum_of_features_final = (/minimum_of_features(1), minimum_of_features(6), minimum_of_features(3),&
         minimum_of_features(2), minimum_of_features(4), minimum_of_features(5)/)

        maximum_of_features_final = (/maximum_of_features(1), maximum_of_features(6), maximum_of_features(3),&
         maximum_of_features(2), maximum_of_features(4), maximum_of_features(5)/)



        normalized_nn_inputs%previous_release = (nn_inputs%previous_release-minimum_of_features_final(1))&
        /(maximum_of_features_final(1)-minimum_of_features_final(1))

        normalized_nn_inputs%previous_storage = (nn_inputs%previous_storage-minimum_of_features_final(2))&
        /(maximum_of_features_final(2)-minimum_of_features_final(2))

        normalized_nn_inputs%inflow = (nn_inputs%inflow-minimum_of_features_final(3))&
        /(maximum_of_features_final(3)-minimum_of_features_final(3))


        normalized_nn_inputs%X_DOY = nn_inputs%X_DOY ! this variable is already
        normalized_nn_inputs%Y_DOY = nn_inputs%Y_DOY

        print *, "normalized_nn_inputs", normalized_nn_inputs



        y_min = minimum_of_features(1)! minimum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!
        y_max = maximum_of_features(1)! maximum of the reservoir release from the pickle file meaning from the dataset that the model has been trained on!


        ! Deallocation of memory

        deallocate(bias)
        deallocate(weights)
        deallocate(minimum_of_features)
        deallocate(maximum_of_features)
        deallocate(activation_functions)
        !deallocate(number_of_nodes)



    end subroutine machine_learning_model_init




    !Machine Learning Model Destructor
    subroutine machine_learning_model_destroy(this)
        implicit none
        class(machine_learning_model_struct), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_model_destroy






    ! This subroutine performs matrix calculations to simulate reservoir release from a saved/trained ML model!
    !subroutine run_machine_learning_model(this)
    subroutine run_machine_learning_model(model_data)
        implicit none
        !class(machine_learning_model_struct), intent(inout) :: this
        class(machine_learning_model_struct), intent(inout) :: model_data
        !real ::



        type (NN_node), dimension (10), intent (IN) :: model_parameters_local

        real, allocatable, dimension(:) :: input_vector_local
        real, allocatable, dimension(:,:) :: reshaped_input_vector_local
        real, intent(OUT) :: ml_simulated_release_local

        type (NN_reservoir_in_out)  :: normalized_nn_inputs_local

        ! Here input combination is assigned!

        input_vector = (/normalized_nn_inputs_local%previous_release, normalized_nn_inputs_local%X_DOY,&
            normalized_nn_inputs_local%Y_DOY, normalized_nn_inputs_local%previous_storage, &
            normalized_nn_inputs_local%inflow/)


        reshaped_input_vector_local = reshape(input_vector, (/ size(input_vector),1/))


        do k=1, (size(number_of_nodes)-1) ! this represents in which layer we stand (i.e., hidden layers+output layer)
            nn_sum_matrix(:,:) = 0.0
            reshaped_nn_sum_matrix(:) = 0.0
            nn_sum_matrix_activated(:)= 0.0
            do i = 1, number_of_nodes(k+1)! nodes;90
                tmp = 0.0
                do j= 1, number_of_nodes(k)
                    if (k==1) then
                        tmp = tmp + model_parameters_local(k)%link_weights(i,j) * reshaped_input_vector_local(j,1) !reshaped_nn_inputs_local(j,1)
                        nn_sum_matrix(i,1) = tmp
                        nn_sum_matrix(i,1) = nn_sum_matrix(i,1) + model_parameters_local(k)%link_bias(i,1)
                        reshaped_nn_sum_matrix(i) = nn_sum_matrix(i,1)
                        if (model_parameters_local(k)%link_activation_functions(1)==0) then ! The focus will probably be on
                            nn_sum_matrix_activated(i) = sigmoid(reshaped_nn_sum_matrix(i))
                        else if (model_parameters_local(k)%link_activation_functions(1)==1) then
                            nn_sum_matrix_activated(i) = relu(reshaped_nn_sum_matrix(i))
                        else
                            nn_sum_matrix_activated(i) = tanh(reshaped_nn_sum_matrix(i))
                        end if
                    else
                        tmp = tmp + model_parameters_local(k)%link_weights(i,j) * reshaped_nn_sum_matrix_activated(j,1)
                        nn_sum_matrix(i,1) = tmp
                        nn_sum_matrix(i,1) = nn_sum_matrix(i,1) + model_parameters_local(k)%link_bias(i,1)
                        reshaped_nn_sum_matrix(i) = nn_sum_matrix(i,1)
                        if (model_parameters_local(k)%link_activation_functions(1)==0) then
                            nn_sum_matrix_activated(i) = sigmoid(reshaped_nn_sum_matrix(i))
                        else if (model_parameters_local(k)%link_activation_functions(1)==1) then
                            nn_sum_matrix_activated(i) = relu(reshaped_nn_sum_matrix(i))
                        else
                            nn_sum_matrix_activated(i) = tanh(reshaped_nn_sum_matrix(i))
                        end if
                    end if

                end do
            end do
            reshaped_nn_sum_matrix_activated = reshape(nn_sum_matrix_activated, (/ size(nn_sum_matrix_activated),1/))
            print *, "k", k

        end do

        normalized_simulated_release = nn_sum_matrix_activated(1)



        ! Simulated release should be denormalized
        ml_simulated_release_local = normalized_simulated_release * (y_max - y_min) + y_min ! first array refers to "Release" values; we could instead introduce it as
        print *, "ml_simulated_release at the original range (cms):",ml_simulated_release_local
        ml_simulated_release = ml_simulated_release_local
        print *, "ml_simulated_release(cms):", ml_simulated_release
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FIXME The corrector terms need to be added here (we already have them in the Python)

        !Calculate the reservoir storage at the current time (i.e., the storage that corresponds to the simulated release); this part will need revision for the operational version

        !FIXME Copied from Python


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!End of the corrector terms

        ! !qo1 = revised_simulated_release ! the revised version of the simulated release is sent back to the same variable within "LEVELPOOL" subroutine!

        contains

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

        !!############################################################################################################################################







    end subroutine run_machine_learning_model



end module module_machine_learning_model
