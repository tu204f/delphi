unit Lb.Pattern;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice;

type
  TParrentCandel = record
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  end;
  TParrentCandelList = TList<TParrentCandel>;

  ///<summary>
  /// Профитная сделка
  ///</summary>
  TTrade = class(TObject)
  private
    FParrentCandels: TParrentCandelList;
    FBuySell: Char;
  public
    constructor Create;
    destructor Destroy; override;
    property BuySell: Char read FBuySell write FBuySell;
    property ParrentCandels: TParrentCandelList read FParrentCandels;
  end;
  TTradeList = TObjectList<TTrade>;

procedure ToCandelParrent(ACandels: TCandelList; AParrent: TParrentCandelList);
procedure StringsToTrade(ATrade: TTrade; AStrings: TStrings);

implementation

procedure ToCandelParrent(ACandels: TCandelList; AParrent: TParrentCandelList);

  function _ToParrentCandel(AClose, ACurrentCandel: TCandel): TParrentCandel;
  var
    xParrent: TParrentCandel;
  begin
    xParrent.Open  := ACurrentCandel.Open / AClose.Close;
    xParrent.High  := ACurrentCandel.High / AClose.Close;
    xParrent.Low   := ACurrentCandel.Low  / AClose.Close;
    xParrent.Close := ACurrentCandel.Close/ AClose.Close;
    xParrent.Vol   := ACurrentCandel.Vol  / AClose.Vol;
    Result := xParrent;
  end;

var
  xClose, xCurrentCandel: TCandel;
  xParrent: TParrentCandel;
  i, iCount: Integer;
begin
  AParrent.Clear;
  iCount := ACandels.Count;
  if iCount > 0 then
  begin
    xClose := ACandels[iCount - 1];
    for i := 0 to iCount - 1 do
    begin
      xCurrentCandel := ACandels[i];
      xParrent := _ToParrentCandel(xClose,xCurrentCandel);
      AParrent.Add(xParrent);
    end;
  end;

end;

procedure StringsToTrade(ATrade: TTrade; AStrings: TStrings);
begin
  AStrings.Clear;
  AStrings.Add(ATrade.BuySell);
  for var xParrent in ATrade.ParrentCandels do
  begin
    AStrings.Add(xParrent.Open.ToString);
    AStrings.Add(xParrent.High.ToString);
    AStrings.Add(xParrent.Low.ToString);
    AStrings.Add(xParrent.Close.ToString);
    AStrings.Add(xParrent.Vol.ToString);
  end;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FParrentCandels := TParrentCandelList.Create;
end;

destructor TTrade.Destroy;
begin
  FreeAndNil(FParrentCandels);
  inherited;
end;

end.
