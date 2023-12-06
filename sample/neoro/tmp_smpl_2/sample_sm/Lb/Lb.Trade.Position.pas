unit Lb.Trade.Position;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils.Candel,
  Lb.Block;


implementation

{ TOpenPosition }

constructor TOpenPosition.Create(const ABlock: TBlock);
begin
  FPriceTop := 0;
  FPriceBottom := 0;
  FBlock := ABlock;
end;

destructor TOpenPosition.Destroy;
begin

  inherited;
end;

end.
