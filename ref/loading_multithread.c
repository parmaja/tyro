/*******************************************************************************************
*
*   raylib example - loading thread
*
*   This example has been created using raylib 1.8 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2014-2018 Ramon Santamaria (@raysan5)
*
********************************************************************************************/

#include "raylib.h"

#include "pthread.h"                        // POSIX style threads management

#include <stdio.h>
#include <stdlib.h>

#define DATA_FILE_SIZE  120                 // Dummy data fyle size in MB

static pthread_t threadId;                  // Loading data thread id
static bool dataLoaded = false;

static void *LoadingThread(void *arg);      // Loading data thread function declaration

int main()
{
    // Initialization
    //--------------------------------------------------------------------------------------
    int screenWidth = 800;
    int screenHeight = 450;

    int error = pthread_create(&threadId, NULL, &LoadingThread, NULL);

    if (error != 0) printf("Error creating loading thread\n");
    else printf("Loading thread initialized successfully\n");

    int framesCounter = 0;

    InitWindow(screenWidth, screenHeight, "raylib example - threads");

    SetTargetFPS(60);
    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!WindowShouldClose())    // Detect window close button or ESC key
    {
        // Update
        //----------------------------------------------------------------------------------
        framesCounter++;
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        BeginDrawing();

            ClearBackground(RAYWHITE);

            if (!dataLoaded) 
            {
                if ((framesCounter/15)%2) DrawText("LOADING DATA...", 240, 200, 40, GRAY);
            }
            else DrawText("DATA LOADED!", 250, 200, 40, RED);

        EndDrawing();
        //----------------------------------------------------------------------------------
    }

    // De-Initialization
    //--------------------------------------------------------------------------------------
    remove("big_data.package");
    
    CloseWindow();        // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

    return 0;
}

// Loading data thread function definition
// NOTE: We simulate data loading by writting to a disk file 
// instead of reading, writting operation takes longer
static void *LoadingThread(void *arg)
{
    FILE *bigFile = fopen("big_data.package", "wb");
    
    unsigned char *data = (unsigned char *)malloc(DATA_FILE_SIZE*1024*1024);
    
    fwrite(data, 1, DATA_FILE_SIZE*1024*1024, bigFile);
    
    fclose(bigFile);
    
    dataLoaded = true;

    return NULL;
}