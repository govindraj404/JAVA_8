using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.itemType.ItemTypes with {
    code 
  //  @UI.Hidden : true
    @Common : {
        Label                    : '{i18n>ITEM_TYPE}',
        Text  : {
            $value                : description,
           ![@UI.TextArrangement]  : #TextOnly
            }
    };
    description @Common : {
        Label        : '{i18n>DESCRIPTION}'
    };
};

annotate cmh.itemType.ItemType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>ITEM_TYPE}',
        CollectionPath : 'ItemTypes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    },Common.ValueListWithFixedValues
);