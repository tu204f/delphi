unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Platfom;

type


  ///<summary>
  /// ��� - ��� ��������
  ///</summary>
  TBot = class(TObject)
  private
    FTradingPlatform: TTradingPlatform;
  protected
    ///<summary>
    /// �������� ����������� ���������� �������� ��������
    ///</summary>
    function IsTrading: Boolean;
    ///<summary>
    /// ���� ������� �������
    ///</summary>
    function IsActivePosition: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetSelected(const ACandels: TCandelList);
    ///<summary>
    /// �� ����� ��������� �������� � ��� ���������
    ///</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
  end;

implementation

{ TBot }

constructor TBot.Create;
begin
  FTradingPlatform := nil;
end;

destructor TBot.Destroy;
begin

  inherited;
end;

function TBot.IsTrading: Boolean;
begin
  // 1. ��������� ��������, ������� ����� ���������
  // 2. ������ ����������� ������
  // 3. ���������� �������� �������� � �� �������� ������
  // 4. ������ ��������� �������� ����� ��������� ������
  Result := True;
end;

procedure TBot.SetSelected(const ACandels: TCandelList);
begin
  if IsTrading then
  begin
    // ����� ��������� �������� ��������
  end
  else if IsActivePosition then
  begin
    // ������������� ��������� �������
  end;
end;

end.
