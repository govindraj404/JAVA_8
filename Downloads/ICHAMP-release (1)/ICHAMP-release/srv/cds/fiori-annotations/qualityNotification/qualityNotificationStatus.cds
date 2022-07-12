using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.qualityNotificationStatus.QualityNotificationStatuses with {
    
    code
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>CODE}';

    name
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>NAME}';

    sequenceNumber
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>SEQUENCE_NUMBER}';

}