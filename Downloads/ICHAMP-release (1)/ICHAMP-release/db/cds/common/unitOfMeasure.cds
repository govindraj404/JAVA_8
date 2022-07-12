namespace com.sap.ic.cmh.unitOfMeasure;

type UnitOfMeasure : Association to one UnitOfMeasures;

entity UnitOfMeasures  {
  key code : String(5);
      ISOCode : String(5);
      name : localized String(45);
}
