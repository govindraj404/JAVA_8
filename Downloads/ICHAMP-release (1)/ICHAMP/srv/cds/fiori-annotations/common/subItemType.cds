using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.subItemType.SubItemTypes with {
    code 
    @Common : {
        Label                    : '{i18n>SUB_ITEM_TYPE}',
        Text  : {
            $value                : description,
           ![@UI.TextArrangement]  : #TextOnly
            }
    };

    description @Common : {
        Label        : '{i18n>DESCRIPTION}'
    };
    
    itemType @Common : {
        Text : itemType.description,
        Label        : '{i18n>ITEM_TYPE}'
    };
}


annotate cmh.subItemType.SubItemType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>SUB_ITEM_TYPE}',
        CollectionPath : 'SubItemTypes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : subItemType_code
            },
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'itemType_code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    }
);