using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.unitOfMeasure.UnitOfMeasures with {
    code
                @Common    : {
        Label : '{i18n>UNIT}',
        Text  : {
            $value                 : name,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    name @Common    : {Label : '{i18n>UNIT}'};

    name
    @UI.HiddenFilter;
};

annotate cmh.unitOfMeasure.UnitOfMeasure with @(
    Common.ValueListMapping : {
        Label          : '{i18n>UNIT}',
        CollectionPath : 'UnitOfMeasures',
        Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'code',
            LocalDataProperty : unit_code
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'name',
            ![@UI.Importance] : #High
        }
        ]
    }
);