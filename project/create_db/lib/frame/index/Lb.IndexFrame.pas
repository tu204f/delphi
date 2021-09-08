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
  Lb.Create.DB, FMX.Layouts;

type
  ///<summary>Индек</summary>
  TIndexFrame = class(TFrame)
  private
    FIndexTable: TCrIndex;
    procedure SetIndexTable(const Value: TCrIndex);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IndexTable: TCrIndex read FIndexTable write SetIndexTable;
  end;

implementation

{$R *.fmx}

{ TIndexFrame }

constructor TIndexFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

end.
