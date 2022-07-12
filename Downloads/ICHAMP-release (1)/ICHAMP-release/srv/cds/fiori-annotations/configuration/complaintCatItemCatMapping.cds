using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintCatItemCatMappings {

    ID
    @UI.Hidden;

    identifier
    @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    complaintCategory
    @Common       : {
        Text             : complaintCategory.name,
        TextArrangement  : #TextOnly,
        FieldControl    : #Mandatory,
        Label            : '{i18n>COMPLAINT_CATEGORY}',
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

    complaintCategory_code @Commom :{
        Label: '{i18n>COMPLAINT_CATEGORY}',
        FieldControl    : #Mandatory
    };

    itemCategory
    @Common       : {
        Text             : itemCategory.description,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>ITEM_CATEGORY}',
        FieldControl    : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>ITEM_CATEGORY}',
            CollectionPath : 'ItemCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'ID',
                    LocalDataProperty : itemCategory_ID
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'code'
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                }
            ]
        }
    };

    itemCategory_ID @Common : {
        Label : '{i18n>ITEM_CATEGORY}',
        FieldControl : #Mandatory
    };

}
