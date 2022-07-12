using ConfigurationService from '../../services/index';

annotate ConfigurationService.BusinessObjectConfigurations with {
    ID
                                 @UI.Hidden;

    identifier
                                 @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    destination                  @Common       : {
        Label            : '{i18n>DESTINATION}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>DESTINATION}',
            CollectionPath : 'Destinations',
            Parameters     : [{
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'destination',
                LocalDataProperty : destination
            }]
        }
    };

    complaintType                @Common       : {
        Label        : '{i18n>COMPLAINT_CATEGORY}',
        FieldControl : #Mandatory
    };

    businessObjectType_code      @Common       : {
        Label            : '{i18n>BUSINESS_OBJECT_TYPE}',
        FieldControl     : #Mandatory,
        Text             : businessObjectAttribute.businessObjectType_code.name,
        TextArrangement  : #TextOnly,
        ValueListMapping : {
            Label          : '{i18n>BUSINESS_OBJECT_TYPE}',
            CollectionPath : 'BusinessObjectTypes',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : businessObjectType_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'name'
                }
            ]
        },
        ValueListWithFixedValues
    };


    businessObjectValue          @Common       : {
        Label        : '{i18n>VALUE}',
        FieldControl : #Mandatory
    };

    complaintType                @Common       : {
        Text            : complaintType.name,
        TextArrangement : #TextOnly
    };

    businessObjectAttribute_code @Common       : {
        Label            : '{i18n>BUSINESS_OBJECT_ATTRIBUTE}',
        FieldControl     : #Mandatory,
        Text             : businessObjectAttribute.name,
        TextArrangement  : #TextOnly,
        ValueListMapping : {
            Label          : '{i18n>BUSINESS_OBJECT_ATTRIBUTES}',
            CollectionPath : 'BusinessObjectAttributes',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'businessObjectAttribute',
                    LocalDataProperty : businessObjectAttribute_code
                },
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'businessObjectType',
                    LocalDataProperty : businessObjectType_code
                }
            ]
        },
        ValueListWithFixedValues
    };
}
