unit uVectorGeometry;

interface

uses
  Vcl.Graphics;

type

  TVector2i = record
    X: Integer;
    Y: Integer;
    constructor Create(_AX, _AY: Integer);
    function Add(_AVector: TVector2i): TVector2i;
    function Subtract(_AVector: TVector2i): TVector2i;
    function Scale(_AFactor: Single): TVector2i;
  end;

  TVector2f = record
    X: Single;
    Y: Single;
    constructor Create(_AX, _AY: Single);
    function Add(_AVector: TVector2f): TVector2f;
    function Subtract(_AVector: TVector2f): TVector2f;
    function Scale(_AFactor: Single): TVector2f;
  end;

  TVector3f = record
    X: Single;
    Y: Single;
    Z: Single;

    constructor Create(_AX, _AY, _AZ: Single);
    function Add(_AVector: TVector3f): TVector3f;
    function Subtract(_AVector: TVector3f): TVector3f;
    function Scale(_AFactor: Single): TVector3f;
    function DotProduct(_AVector: TVector3f): Single;
    function Magnitude: Single;
    function Normalize: TVector3f;
    function Reflect(_ANormal: TVector3f): TVector3f;
    function Refract(_AN: TVector3f; eta_t: Single; eta_i: Single = 1): TVector3f;
    function CrossProduct(_AVector: TVector3f): TVector3f;
  end;

  TVector3i = record
    X: Integer;
    Y: Integer;
    Z: Integer;
    constructor Create(_AX, _AY, _AZ: Integer);
    function Add(_AVector: TVector3i): TVector3i;
    function Subtract(_AVector: TVector3i): TVector3i;
    function Scale(_AFactor: Integer): TVector3i;
  end;


  TFaceVertex = record
    IDPosition: Integer;
    IDUV: Integer;
    IDNormal: Integer;

    constructor Create(_AIdPosition, _AIdUV, _AIdNormal: Integer);
  end;

  TVector4f = record
    X: Single;
    Y: Single;
    Z: Single;
    W: Single;
    constructor Create(_AX, _AY, _AZ, _AW: Single);
  end;

  TLight = record
    Position: TVector3f;
    Intensity: Single;
    constructor Create(_APosition: TVector3f; _AIntensity: Single);
  end;


  //function GetColor(R, G, B: Single): TColor; overload;
  function GetColor(_AColor: TColor): TVector3f; overload;
  function GetColor(_AVectorColor: TVector3f): TColor; overload; //Remover isto
  function Barycentric(_APts: TArray<TVector3f>; _AP: TVector3f): TVector3f;
  function Vector3fTo3i(_AV: TVector3f): TVector3i;

implementation

uses
  Winapi.Windows, System.SysUtils, Math;

function Vector3fTo3i(_AV: TVector3f): TVector3i;
begin
  result := TVector3i.Create( trunc(_AV.X),
                              trunc(_AV.Y),
                              trunc(_AV.Z));
end;

function GetColor(_AColor: TColor): TVector3f; overload;
begin
  result := TVector3f.Create( GetRValue(_AColor)/255,
                              GetGValue(_AColor)/255,
                              GetBValue(_AColor)/255);
end;
//function GetColor(R, G, B: Single): TColor;
//var
//  AR,
//  AG,
//  AB: Byte;
//begin
//  if (R < 0) or (R > 1) or
//     (G < 0) or (G > 1) or
//     (B < 0) or (B > 1) then
//     raise Exception.Create('Invalid parameter.');
//
//  AR := Trunc(R * 255);
//  AG := Trunc(G * 255);
//  AB := Trunc(B * 255);
//  Result := RGB(AR, AG, AB);
//end;

//function Barycentric(_APts: TArray<TVector3f>; _AP: TVector3f): TVector3f;
//var
//	u: TVector3f;
//	A, B: TVector3f;
//begin
//  A.x := _APts[2].x - _APts[0].x;
//  A.y := _APts[1].x - _APts[0].x;
//  A.z := _APts[0].x - _AP.x;
//
//  B.x := _APts[2].y - _APts[0].y;
//  B.y := _APts[1].y - _APts[0].y;
//  B.z := _APts[0].y - _AP.y;
//
//	u := A.CrossProduct(B);
//
//	if Abs(u.Z) > 0.001 then
//	begin
//	  Result := TVector3f.Create(1 - (u.x + u.y) / u.z, u.y / u.z, u.x / u.z);
//		exit;
//	end;
//
//	Result := TVector3f.Create(-1, 1, 1);
//end;

function Barycentric(_APts: TArray<TVector3f>; _AP: TVector3f): TVector3f;
var
	u: TVector3f;
	A, B: TVector3f;
begin
	A := TVector3f.Create(_APts[2].X - _APts[0].X,
                        _APts[1].X - _APts[0].X,
                        _APts[0].X - _AP.X);

	B := TVector3f.Create(_APts[2].Y - _APts[0].Y,
                        _APts[1].Y - _APts[0].Y,
                        _APts[0].Y - _AP.Y);

	u := A.CrossProduct(B);

	if Abs(u.Z) < 1 then
	begin
		Result := TVector3f.Create(-1, 1, 1);
		exit;
	end;

	Result := TVector3f.Create(1 - (u.x + u.y) / u.z, u.y / u.z, u.x / u.z);
end;

function GetColor(_AVectorColor: TVector3f): TColor;
var
  AR,
  AG,
  AB: Integer;
  function FixMaxRange(_AValue: Integer): Integer;
  begin
    Result:= _AValue;
    if _AValue > 255 then
      Result := 255;
  end;
  function FixMinRange(_AValue: Integer): Integer;
  begin
    Result:= _AValue;
    if _AValue < 0 then
      Result := 0;
  end;
begin
  AR := Trunc(_AVectorColor.X * 255);
  AG := Trunc(_AVectorColor.Y * 255);
  AB := Trunc(_AVectorColor.Z * 255);

  AR := FixMaxRange(AR);
  AR := FixMinRange(AR);

  AG := FixMaxRange(AG);
  AG := FixMinRange(AG);

  AB := FixMaxRange(AB);
  AB := FixMinRange(AB);

  Result := RGB(AR, AG, AB);
end;

{ TVector3f }

function TVector3f.Add(_AVector: TVector3f): TVector3f;
begin
  Result := TVector3f.Create(X + _AVector.X,
                             Y + _AVector.Y,
                             Z + _AVector.Z);
end;

constructor TVector3f.Create(_AX, _AY, _AZ: Single);
begin
  X := _AX;
  Y := _AY;
  Z := _AZ;
end;

function TVector3f.CrossProduct(_AVector: TVector3f): TVector3f;
begin
  Result := TVector3f.Create(Y * _AVector.Z - Z * _AVector.Y,
                             Z * _AVector.X - X * _AVector.Z,
                             X * _AVector.Y - Y * _AVector.X);
end;

function TVector3f.DotProduct(_AVector: TVector3f): Single;
begin
  Result := X * _AVector.X +
            Y * _AVector.Y +
            Z * _AVector.Z;
end;

function TVector3f.Magnitude: Single;
begin
  Result := Sqrt(X * X +
                 Y * Y +
                 Z * Z);
end;

function TVector3f.Normalize: TVector3f;
var
  AMag: Single;
begin
  AMag := Magnitude;
  Result := TVector3f.Create(X / AMag,
                             Y / AMag,
                             Z / AMag);
end;

function TVector3f.Reflect(_ANormal: TVector3f): TVector3f;
var
  AIDotN: Single;
  ANScale2: TVector3f;
begin
  //Result := I - N*2.f*(I*N);
  ANScale2 := _ANormal.Scale(2);
  AIDotN := Self.DotProduct(_ANormal);

  Result := Self.Subtract(ANScale2.Scale(AIDotN));
end;

function TVector3f.Refract(_AN: TVector3f;
  eta_t: Single; eta_i: Single = 1): TVector3f;
var
  cosi: Single;
  eta: Single;
  k: Single;
begin
  cosi := Max(-1, Min(1, Self.DotProduct(_AN))) * (-1);
  if (cosi < 0) then
  begin
    result := Self.Refract(_AN.Scale(-1), eta_i, eta_t);
    exit;
  end;

  eta := eta_i / eta_t;
  k := 1 - eta * eta * (1 - cosi * cosi);

  if (k < 0) then
    result := TVector3f.Create(1, 0, 0)
  else
    result := Self.Scale(eta).Add(_AN.Scale(eta * cosi - sqrt(k)));
end;

function TVector3f.Scale(_AFactor: Single): TVector3f;
begin
  Result := TVector3f.Create(X * _AFactor,
                             Y * _AFactor,
                             Z * _AFactor);
end;

function TVector3f.Subtract(_AVector: TVector3f): TVector3f;
begin
  Result := TVector3f.Create(X - _AVector.X,
                             Y - _AVector.Y,
                             Z - _AVector.Z);
end;

{ TLight }

constructor TLight.Create(_APosition: TVector3f; _AIntensity: Single);
begin
  Position := _APosition;
  Intensity := _AIntensity;
end;

{ TVector2f }

constructor TVector4f.Create(_AX, _AY, _AZ, _AW: Single);
begin
  X := _AX;
  Y := _AY;
  Z := _AZ;
  W := _AW;
end;

{ TVector2f }

function TVector2f.Add(_AVector: TVector2f): TVector2f;
begin
  Result := TVector2f.Create(X + _AVector.X,
                             Y + _AVector.Y);
end;

constructor TVector2f.Create(_AX, _AY: Single);
begin
  X := _AX;
  Y := _AY;
end;

function TVector2f.Scale(_AFactor: Single): TVector2f;
begin
  Result := TVector2f.Create(Trunc(X * _AFactor),
                             Trunc(Y * _AFactor));
end;

function TVector2f.Subtract(_AVector: TVector2f): TVector2f;
begin
  Result := TVector2f.Create(X - _AVector.X,
                             Y - _AVector.Y);
end;

{ TVector2i }

function TVector2i.Add(_AVector: TVector2i): TVector2i;
begin
  Result := TVector2i.Create(X + _AVector.X,
                             Y + _AVector.Y);
end;

constructor TVector2i.Create(_AX, _AY: Integer);
begin
  X := _AX;
  Y := _AY;
end;

function TVector2i.Scale(_AFactor: Single): TVector2i;
begin
  Result := TVector2i.Create(Trunc(X * _AFactor),
                             Trunc(Y * _AFactor));
end;

function TVector2i.Subtract(_AVector: TVector2i): TVector2i;
begin
  Result := TVector2i.Create(X - _AVector.X,
                             Y - _AVector.Y);
end;

{ TFaceVertex }

constructor TFaceVertex.Create(_AIdPosition, _AIdUV, _AIdNormal: Integer);
begin
  IDPosition := _AIdPosition;
  IDUV := _AIdUV;
  IDNormal := _AIdNormal;
end;

{ TVector3i }

function TVector3i.Add(_AVector: TVector3i): TVector3i;
begin
  Result := TVector3i.Create(X + _AVector.X,
                             Y + _AVector.Y,
                             Z + _AVector.Z);
end;

constructor TVector3i.Create(_AX, _AY, _AZ: Integer);
begin
  X := _AX;
  Y := _AY;
  Z := _AZ;
end;

function TVector3i.Scale(_AFactor: Integer): TVector3i;
begin
  Result := TVector3i.Create(X * _AFactor,
                             Y * _AFactor,
                             Z * _AFactor);
end;

function TVector3i.Subtract(_AVector: TVector3i): TVector3i;
begin
  Result := TVector3i.Create(X - _AVector.X,
                             Y - _AVector.Y,
                             Z - _AVector.Z);
end;

end.
