/*
 * Copyright © 2021-2022, David Priver
 */
module dlib.bmp;
struct BmpHeader {
    uint   bfSize;
    uint   bfReserved;
    uint   bfOffBits;
    uint   biSize;
    uint   biWidth;
    int    biHeight;
    ushort biPlanes;
    ushort biBitCount;
    uint   biCompression;
    uint   biSizeImage;
    int    biXPelsPerMeter;
    int    biYPelsPerMeter;
    uint   biClrUsed;
    uint   biClrImportant;
}
static assert(BmpHeader.sizeof==52);
size_t 
PAD(size_t a){
    return (a + ( (4 - (a % 4) ) % 4) );
}

struct BmpPixel {
    ubyte blue;
    ubyte green;
    ubyte red;
};

BmpPixel 
rgb(ubyte r, ubyte g, ubyte b){
    return BmpPixel(b, g, r);
}

uint 
abs(int x){
    return x < 0? -x : x;
}

struct BmpImg {
    BmpHeader header;
    BmpPixel[0] pixels_;
    inout(BmpPixel[]) pixels() return inout{
        return pixels_.ptr[0 .. header.biWidth * abs(header.biHeight)];
    }
    ref BmpPixel 
    opIndex(ulong x, ulong y){
        uint h = abs(header.biHeight);
        uint w = header.biWidth;
        size_t index = y * w + x;
        return pixels[index];
    }
    void
    opIndexAssign(BmpPixel pixel, ulong x, ulong y){
        uint h = abs(header.biHeight);
        uint w = header.biWidth;
        size_t index = y * w + x;
        pixels[index] = pixel;
    }
    static
    size_t
    size_for(int w, int h){
        return header.sizeof + BmpPixel.sizeof * abs(w)*abs(h);
    }

    static
    BmpImg*
    make(int w, int h, void[] mem){
        if(mem.length < size_for(w, h)) return null;
        BmpImg* result = cast(BmpImg*)mem.ptr;
        BmpHeader* header = &result.header;
        size_t padded_width = PAD(abs(w) * BmpPixel.sizeof);
        size_t padded_size = abs(h) * padded_width;
        header.bfSize = cast(uint)(padded_size + BmpHeader.sizeof + 2);
        header.bfReserved = 0;
        header.bfOffBits = 54;
        header.biSize = 40;
        header.biWidth = abs(w);
        header.biHeight = h;
        header.biPlanes = 1;
        header.biBitCount = 24;
        header.biCompression = 0;
        header.biSizeImage = 0;
        header.biXPelsPerMeter = 0;
        header.biYPelsPerMeter = 0;
        header.biClrUsed = 0;
        header.biClrImportant = 0;
        return result;
    }
}

void
fill_square(BmpImg* img, uint x, uint y, uint w, uint h, BmpPixel p){
    for(uint dy = 0; dy < h; dy++)
    for(uint dx = 0; dx < w; dx++){
        (*img)[x+dx, y+dy] = p;
    }
}

void
horizontal(BmpImg* img, uint x, uint y, uint length, BmpPixel p){
    for(uint dx = 0; dx < length; dx++){
        (*img)[x+dx, y] = p;
    }
}
void
vertical(BmpImg* img, uint x, uint y, uint length, BmpPixel p){
    for(uint dy = 0; dy < length; dy++){
        (*img)[x, y+dy] = p;
    }
}

void
write_bmp(Out)(const BmpImg* img, Out sink){
    auto header = &img.header;
    ubyte[2] magic = [0x42, 0x4D];
    sink.write(magic);
    sink.write(header[0..1]);
    uint h = abs(img.header.biHeight);
    ubyte[3] padding = [0, 0, 0];
    auto padding_amount = (4 - ( (img.header.biWidth * BmpPixel.sizeof ) % 4 ) ) % 4;
    auto row_width = header.biWidth;
    for(size_t y = 0; y < h; y++){
        // Write a whole row of pixels to the file:
        const BmpPixel[] pix = img.pixels[row_width*y .. row_width*(y+1)];
        sink.write(pix);
        if(padding_amount)
            sink.write(padding[0..padding_amount]);
    }
}
