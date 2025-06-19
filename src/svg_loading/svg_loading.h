/*******************************************************************************************
*
*   raylib [textures] example - SVG loading and texture creation
*
*   NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
*
*   Example originally created with raylib 4.2, last time updated with raylib 5.5
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2022-2024 Dennis Meinen (@bixxy#4258 on Discord) and Ramon Santamaria (@raysan5)
*
********************************************************************************************/

#include "raylib.h"

#define NANOSVG_IMPLEMENTATION          // Expands implementation
#include "nanosvg.h"

#define NANOSVGRAST_IMPLEMENTATION
#include "nanosvgrast.h"


// Load SVG image, rasteraizing it at desired width and height
// NOTE: If width/height are 0, using internal default width/height
static Image LoadImageSVG(const char *fileName, int width, int height)
{
    Image image = { 0 };

    if ((strcmp(GetFileExtension(fileName), ".svg") == 0) ||
        (strcmp(GetFileExtension(fileName), ".SVG") == 0))
    {
        int dataSize = 0;
        unsigned char *fileData = NULL;

        fileData = LoadFileData(fileName, &dataSize);

        // Make sure the file data contains an EOL character: '\0'
        if ((dataSize > 0) && (fileData[dataSize - 1] != '\0'))
        {
            fileData = RL_REALLOC(fileData, dataSize + 1);
            fileData[dataSize] = '\0';
            dataSize += 1;
        }

        struct NSVGimage *svgImage = nsvgParse(fileData, "px", 96.0f);

        float scale;

        // If one dimension is 0 scale proportional
        if (width == 0) {
            width = height * svgImage->width/svgImage->height;
            scale = height/svgImage->height;
        } else if (height == 0) {
            height = width * svgImage->height/svgImage->width;
            scale = width/svgImage->width;
        } else {
            float scaleWidth = width/svgImage->width;
            float scaleHeight = height/svgImage->height;
            scale = (scaleHeight > scaleWidth)? scaleWidth : scaleHeight;
        }
        
        unsigned char *imgData = RL_MALLOC(width*height*4);

        // Rasterize
        struct NSVGrasterizer *rast = nsvgCreateRasterizer();
        nsvgRasterize(rast, svgImage, 0, 0, scale, imgData, width, height, width*4);

        // Populate image struct with all data
        image.data = imgData;
        image.width = width;
        image.height = height;
        image.mipmaps = 1;
        image.format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8;

        nsvgDelete(svgImage);
        nsvgDeleteRasterizer(rast);
        UnloadFileData(fileData);
    }

    return image;
}