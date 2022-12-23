unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  ///<summary>����� ������<summary>
  TTradeStream = class(TObject)
  public type
    TPrice = record
      Price: Double;
      Value: Double;
    end;
    TPriceList = TList<TPrice>;
  private
    FPriceBegin: Double; // ���� ������
    FPriceLast: Double;  // ������� ����
    FCount: Integer;     // ���������� ���������, ��� ������
    FValue: Double;
    FPrices: TPriceList;
    function GetWeight: Double;      // ����� ������� ������
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //procedure AddPrice(const APrice, AValue: Double);
    property PriceBegin: Double read FPriceBegin write FPriceBegin;
    property PriceLast: Double read FPriceLast write FPriceLast;
    property Count: Integer read FCount write FCount;
    ///<summary>��� ������</summary>
    property Weight: Double read GetWeight;
  end;

  ///<summary>������ �����</summary>
  TTradeStreamList = TObjectList<TTradeStream>;

  ///<summary>������� ������� �������� ������ ��������</summary>
  TTrigger = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  ///<summary>����� ���</summary>
  TLifeBot = class(TObject)
  private
    FPosition: Boolean;
  protected
    FTiket: TTiket;
    FOpenTiket: TTiket;
    FCloseTilet: TTiket;
    function IsOpenPosition: Boolean;
    function IsClosePosition: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //����������� ����� �������� �����
    procedure SetTiket(const ATiket: TTiket);
  end;

implementation

{ TTradeStream }

constructor TTradeStream.Create;
begin
  FPriceBegin := 0;
  FPriceLast := 0;
  FCount := 0;
  FValue := 0;
end;

destructor TTradeStream.Destroy;
begin

  inherited;
end;

function TTradeStream.GetWeight: Double;
begin
  Result := 0;
  if FCount > 0 then
    Result := 1/FCount;
end;

{ TTrigger }

constructor TTrigger.Create;
begin

end;

destructor TTrigger.Destroy;
begin

  inherited;
end;

{ TLifeBot }

constructor TLifeBot.Create;
begin
  FPosition := False;
end;

destructor TLifeBot.Destroy;
begin

  inherited;
end;

procedure TLifeBot.SetTiket(const ATiket: TTiket);
begin
  FTiket := ATiket;
  if FPosition then
  begin
    if IsClosePosition then
    begin
      FCloseTilet := ATiket;
      FPosition := False;
    end;
  end
  else
  begin
    if IsOpenPosition then
    begin
      FOpenTiket := ATiket;
      FPosition := True;
    end;
  end;
end;

function TLifeBot.IsOpenPosition: Boolean;
begin
  // �������� ������� � ��������� ���������� �����

end;

function TLifeBot.IsClosePosition: Boolean;
begin
  // ��������� ������� �� ����������� �������

end;





end.
