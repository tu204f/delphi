unit Lb.ModuleTabItem;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Objects,
  FMX.Layouts,
  FMX.Forms,
  FMX.TabControl;

type
  TModuleTabItem = class(TTabItem)
  private
    FFrame: TFrame;
    FObjectKey: String;
    FCaption: String;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    procedure SetFrame(const Value: TFrame);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ObjectKey: String read FObjectKey write FObjectKey;
    property Caption: String read GetCaption write SetCaption;
    property Frame: TFrame read FFrame write SetFrame;
  end;

implementation

{ TModuleTabItem }

constructor TModuleTabItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TModuleTabItem.Destroy;
begin
  if Assigned(FFrame) then
    FreeAndNil(FFrame);
  FFrame := nil;
  inherited;
end;

function TModuleTabItem.GetCaption: String;
begin
  Result := FCaption;
end;

procedure TModuleTabItem.SetCaption(const Value: String);
begin
  FCaption := Value;
end;

procedure TModuleTabItem.SetFrame(const Value: TFrame);
begin
  FFrame := Value;
  if Assigned(FFrame) then
  begin
    FFrame.Parent := Self;
    FFrame.Align := TAlignLayout.Client;
  end;
end;

end.
