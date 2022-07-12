using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.streamType.StreamTypes with {
    code
    @Common : {Text : {
        $value                 : name,
        ![@UI.TextArrangement] : #TextOnly
    }};

    @Common.FieldControl  : #ReadOnly
    name;
}
