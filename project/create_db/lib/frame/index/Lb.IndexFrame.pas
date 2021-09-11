unit Lb.IndexFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Create.DB,
  FMX.Layouts,
  Lb.SysUtils,
  Lb.WinFrame;

type
  ///<summary>Индек</summary>
  TIndexFrame = class(TFrame, IWinModule)
  private
    FIndexTable: TCrIndex;
    procedure SetIndexTable(const Value: TCrIndex);
  private
    FStatus: TStatusFrame;
    function GetCode: WideString;
    function GetStatus: TStatusFrame;
    procedure SetStatus(const AStatus: TStatusFrame);
  protected
    procedure SetApply;
    procedure SetClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IndexTable: TCrIndex read FIndexTable write SetIndexTable;
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

implementation

{$R *.fmx}

{ TIndexFrame }

constructor TIndexFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := TAlignLayout.Client;
  FIndexTable := nil;
end;

destructor TIndexFrame.Destroy;
begin

  inherited;
end;

procedure TIndexFrame.SetIndexTable(const Value: TCrIndex);
begin
  FIndexTable := Value;
end;

function TIndexFrame.GetCode: WideString;
begin
  Result := Self.ClassName;
end;


function TIndexFrame.GetStatus: TStatusFrame;
begin
  Result := FStatus;
end;

procedure TIndexFrame.SetStatus(const AStatus: TStatusFrame);
begin
  FStatus := AStatus;
end;

procedure TIndexFrame.SetApply;
begin
  // Применить изменение
end;

procedure TIndexFrame.SetClose;
begin
  // Отменить заявки
end;


end.
