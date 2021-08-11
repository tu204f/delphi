unit Lb.Level;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Ценовой уровень - заявка</summary>
  ///<remarks></remarks>
  TLevel = class(TObject)
  private
    FOrderNo: Int64;
    FPrice: Double;
    FBuySell: Char;
    FBalance: Integer;
    FQuantity: Integer;
    FActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Price: Double read FPrice write FPrice;
    ///<summary>Количество ожидающие испольнение</summary>
    property Balance: Integer read FBalance write FBalance;
    property Quantity: Integer read FQuantity write FQuantity;
    property BuySell: Char read FBuySell write FBuySell;
    property Active: Boolean read FActive write FActive;
  end;
  TLevelList = TObjectList<TLevel>;

  ///<summary>Список уровней</summary>
  TLevels = class(TObject)
  private
    FItems: TLevelList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Items: TLevelList read FItems;
    property Count: Integer read GetCount;
  end;

implementation

{ TLevel }

constructor TLevel.Create;
begin

end;

destructor TLevel.Destroy;
begin

  inherited;
end;

{ TLevels }

constructor TLevels.Create;
begin
  FItems := TLevelList.Create;
end;

destructor TLevels.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TLevels.GetCount: Integer;
begin
  Result := FItems.Count;
end;

end.
