using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.serviceMaterialDestination.ServiceMaterialDestinations with {

	ID
	@UI.Hidden : true;

	itemType
    @UI.Hidden : true;

    subItemType
    @UI.Hidden : true;

	destinationConfigurationID
	@UI.Hidden : true;

	companyCode
	@UI.Hidden : true;

	destination
	@UI.Hidden : true;

	serviceMaterial
	@UI.Hidden : true;

	unit
	@Common.Label : '{i18n>UNIT_CODE}';
};