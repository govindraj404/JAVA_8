namespace com.sap.ic.cmh.common.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ START OF Address.cds ~~~~~~~~~~~~~~~~~~~~~~  */

type Address : String(10);
type Street : String(60);
type HouseNumber : String(10);
type AddressLine1 : String(60);
type AddressLine2 : String(60);
type AddressLine3 : String(60);
type PostalCode : String(10);
type City : String(40);
type Region : String(2);
type CountryKey : String(3);
type PoBox : String(10);
type Mobile : String(30);
type Telephone : String(30);
type Extension : String(10);
type FaxNumber : String(31);
type Email : String(241);
type ContactPerson : String(60);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Address.cds ~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF BusinessPartner.cds ~~~~~~~~~~~~~~~~~~~  */

type BusinessPartner : String(10);
type BusinessPartnerNumber : String(40);
type BusinessPartnerName : String(40);
type VendorNumber : String(10);
type CustomerNumber : String(10);
type DeletionFlag : Boolean;
type CentralBlock : Boolean;
type BusinessPartnerType : String(10);
type PurchagingOrganization : String(10);
type BusinessPartnerName1 : String(40);
type BusinessPartnerName2 : String(40);
type VendorCode : String(10);
type CustomerCode : String(10);
type IsMarkedForDeletion : Boolean;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF BusinessPartner.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ START OF CompanyCode.cds ~~~~~~~~~~~~~~~~~~~~~~  */

type CompanyCode : String(4);
type CompanyCodeName : String(25);
type Country : String(15);
type Currency : String(5);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF CompanyCode.cds ~~~~~~~~~~~~~~~~~~~~~~~  */ 
/* ~~~~~~~~~~~~~~~~~~~~~~ START OF DistributionChannel.cds ~~~~~~~~~~~~~~~~~~  */ 
 
type DistributionChannel         : String(2);
type DistributionChannelName     : String(20);

/* ~~~~~~~~~~~~~~~~~~~~~~~ END OF DistributionChannel.cds ~~~~~~~~~~~~~~~~~~~  */ 
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF Division.cds ~~~~~~~~~~~~~~~~~~~~~~~~  */ 

type SalesDivision           : String(2);
type SalesDivisionName       : String(20);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Division.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */ 

/* ~~~~~~~~~~~~~~~~~~~ START OF MaterialMasterGeneralData.cds ~~~~~~~~~~~~~~~~  */

type MaterialMaster : String(40);
type MaterialCode : String(40);
type MaterialDescription : String(40);
type MaterialType : String(4);
type MaterialGroup : String(9);
type BaseUnitOfMeasure : String(3);
type XPlantMaterialStatus : String(2);
type ValidFrom : Date;
type GrossWeight : Decimal(13, 3);
type NetWeight : Decimal(13, 3);
type Volume : Decimal(13, 3);
type SizeDimensions : String(32);
type ProductCompositionIndicator : String(1);
type PackagingMaterialGroup : String(4);
type Division : String(2);


/* ~~~~~~~~~~~~~~~~~~~~~~~ END OF MaterialMasterGeneralData.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~ START OF MaterialMasterGeneralDataExtension.cds ~~~~~~~~~~~~~~  */

type MaterialGroupPackaging : String(4);
type ProductComposition : String(1);
type Block : String(40);
type EAN_UPC : String(18);
type EANCategary : String(2);

/* ~~~~~~~~~~~~~~~~~~~ END OF MaterialMasterGeneralDataExtension.cds ~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~ START OF MaterialMasterPlantData.cds ~~~~~~~~~~~~~~~~~~~~  */

type TemperatureConditions : String(10);
type StorageConditions : String(40);
type ContainerRequirements : String(2);
type SerialNumberProfile : String(4);
type MerialNumberProfile : String(4);
type PlantSpecificStatus : String(40);
type PricingIndicator : String(1);
type StandardPrice : String(3);
type MovingAveragePrice : String(3);
type Price : Decimal(11, 2);

/* ~~~~~~~~~~~~~~~~~~~~~~~ END OF MaterialMasterPlantData.cds ~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~ START OF MaterialMasterMRPData.cds ~~~~~~~~~~~~~~~~~~~~~  */

type ReorderPoint : String(13);
type MrpType : String(3);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF MaterialMasterMRPData.cds ~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF Plant.cds ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  */

type Plant : String(4);
type PlantName : String(25);
type PlantNameExtension : String(40);
type FactoryCalendar : String(2);
type SupplierNoOfPlant : String(10);
type CustomerNoOfPlant : String(10);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Plant.cds ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF PurchaseOrganization.cds ~~~~~~~~~~~~~~~~~~~~~~~~  */

type PurchaseOrganization : String(4);
type PurchaseOrganizationKey : String(3);
type PurchaseOrganizationName : String(25);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF PurchaseOrganization.cds ~~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF StorageLocation.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */

type StorageLocationCode : String(4);
type storageLocation : String(4);
type storageLocationName : String(25);
type PlantCode : String(4);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF StorageLocation.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF SalesOrganization.cds ~~~~~~~~~~~~~~~~~~~~~~~~  */ 

type SalesOrganization                  : String(4);
type SalesOrganizationName              : String(25);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SalesOrganization.cds ~~~~~~~~~~~~~~~~~~~~~~~~  */ 

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF ErrorLog.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */ 
type Destination : String(30);
type ErrorMessage : String;
type OperationType : String;
type CreatedAt : Timestamp;
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF ErrorLog.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF complaintCategory.cds ~~~~~~~~~~~~~~~~~~~  */

type ComplaintCategoryCode : String(6);
type ComplaintCategoryName : String(40);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF complaintCategory.cds ~~~~~~~~~~~~~~~~~~~~  */
