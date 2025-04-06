unit Lb.Bot.Candel;

interface

{$I debug_volt.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Journal.Trading,
  Lb.CustomWorkBot;

type
  ///<summary>
  /// ������� ���
  ///</summary>
  ///<remarks>
  /// �������� ���� ���� ����� ������
  ///</remarks>
  TWorkBot = class(TCustomWorkBot)
  private
    FCloseTriling: Double;
  protected
    function GetInfoValue: String; override;
    procedure SetCloseCurrentPosition(const APosition: TJournalPosition); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// ����� �����
    ///</summary>
    procedure TradingNewCandel; override;
    ///<summary>
    /// ������� ���������� ������, ���������
    ///</summary>
    procedure TradingUpDataCandel(const ATradingPlatform: TTradingPlatform); override;
  public
    ///<summary>
    /// ������ �������� �������, ����� ��������
    ///</summary>
    property CloseTriling: Double read FCloseTriling write FCloseTriling;
  end;

  ///<summary>
  /// ������ �������
  ///</summary>
  TWorkBotList = TObjectList<TWorkBot>;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;

{ TWorkBot }

constructor TWorkBot.Create;
begin
  FCloseTriling := 10;
end;

destructor TWorkBot.Destroy;
begin
  inherited;
end;

function TWorkBot.GetInfoValue: String;
begin
  Result := 'tr_' + FCloseTriling.ToString;
end;

procedure TWorkBot.SetCloseCurrentPosition(const APosition: TJournalPosition);
begin
  inherited;
  if Assigned(APosition) then
    with APosition do
    begin
      //RatesSL := 0.5;
      //RatesTK := 3;
      Triling := Self.CloseTriling;
    end;
end;

procedure TWorkBot.TradingNewCandel;
begin
  inherited TradingNewCandel;
end;

procedure TWorkBot.TradingUpDataCandel(const ATradingPlatform: TTradingPlatform);
begin
  inherited TradingUpDataCandel(ATradingPlatform);
end;

end.
