using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.businessObjectType.BusinessObjectTypes with {
    code 
    @UI.Hidden : true
    @Common : {
        Label                    : '{i18n>BUSINESS_OBJECT_TYPE}',
        Text  : {
            $value                : name,
           ![@UI.TextArrangement]  : #TextOnly
            }
    };
    @Common.FieldControl  : #ReadOnly
    name @Common : {
        Label        : '{i18n>BUSINESS_OBJECT_TYPE}'
    };
}


annotate cmh.businessObjectType.BusinessObjectType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>BUSINESS_OBJECT_TYPES}',
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
    },Common.ValueListWithFixedValues
);