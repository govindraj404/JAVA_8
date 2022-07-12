using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeConfigurations {

    ID
    @UI.Hidden;

    isFieldControlIndividualComplaintType
    @UI.Hidden;

    identifier
    @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    code
    @Common       : {
        Label        : '{i18n>COMPLAINT_TYPE}',
        FieldControl : #Mandatory
    };

    description
    @Common.Label : '{i18n>DESCRIPTION}';

    complaintCategory
    @Common       : {
        Text             : complaintCategory.name,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>COMPLAINT_CATEGORY}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>COMPLAINT_CATEGORY}',
            CollectionPath : 'ComplaintCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : complaintCategory_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'name'
                }
            ]
        }
    };

    complaintCategory_code
    @Common.Label : '{i18n>COMPLAINT_CATEGORY}';


    individualComplaintType
    @Common.Label : '{i18n>INDIVIDUAL_COMPLAINT}';

    itemCategory
    @Common       : {
        FieldControl     : isFieldControlIndividualComplaintType,
        Text             : itemCategory.description,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>DEFAULT_ITEM_CATEGROY}',
        ValueListMapping : {
            Label          : '{i18n>DEFAULT_ITEM_CATEGROY}',
            CollectionPath : 'ItemCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'ID',
                    LocalDataProperty : itemCategory_ID
                },
                {
                    $Type             : 'Common.ValueListParameterIn',
                    ValueListProperty : 'complaintCategory_code',
                    LocalDataProperty : complaintCategory_code
                },
                {
                    $Type             : 'Common.ValueListParameterConstant',
                    ValueListProperty : 'individualComplaintType',
                    Constant          : 'false'
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'code'
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                },
                 {
                    $Type             : 'Common.ValueListParameterConstant',
                    ValueListProperty : 'isActive',
                    Constant          : 'true'
                }
            ]
        }
    };

    itemCategory_ID
    @Common       : {
        Text            : itemCategory.description,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DEFAULT_ITEM_CATEGROY}'
    };
     isInActive
     @UI.Hidden;

     isActive
     @Common.Label : '{i18n>ACTIVE}';
}
