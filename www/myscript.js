
    let stream;
    let capturedImageData;
    
    // Connect the start camera button
    $(document).on("click", "#startCamera", function() {
      initCamera();
    });
    
    // Function to initialize the camera
    async function initCamera() {
      try {
        // Stop any existing stream first
        if (stream) {
          stream.getTracks().forEach(track => track.stop());
        }
        
        const constraints = {
          video: {
            facingMode: "environment", // Use back camera by default
            width: { ideal: 1280 },
            height: { ideal: 720 }
          }
        };
        
        stream = await navigator.mediaDevices.getUserMedia(constraints);
        const videoElement = document.getElementById("camera");
        videoElement.srcObject = stream;
        videoElement.style.display = "block";
        
        Shiny.setInputValue("cameraStatus", "Camera ready");
        Shiny.setInputValue("cameraActive", true);
      } catch (err) {
        console.error("Error accessing camera:", err);
        Shiny.setInputValue("cameraStatus", "Error: " + err.message);
        Shiny.setInputValue("cameraActive", false);
      }
    }
    
    // Function to capture photo
    function capturePhoto() {
      try {
        const video = document.getElementById("camera");
        const canvas = document.createElement("canvas");
        
        // Set canvas dimensions to match the video
        canvas.width = video.videoWidth;
        canvas.height = video.videoHeight;
        
        // Draw the current video frame to the canvas
        const context = canvas.getContext("2d");
        context.drawImage(video, 0, 0, canvas.width, canvas.height);
        
        // Convert canvas to data URL (base64)
        capturedImageData = canvas.toDataURL("image/jpeg", 0.9);
        
        // Send the image data to R
        Shiny.setInputValue("capturedImage", capturedImageData);
        Shiny.setInputValue("cameraStatus", "Photo captured");
        
        return capturedImageData;
      } catch (err) {
        console.error("Error capturing photo:", err);
        Shiny.setInputValue("cameraStatus", "Error capturing: " + err.message);
        return null;
      }
    }
    
    // Connect the capture button
    $(document).on("click", "#capture", function() {
      capturePhoto();
    });
    
    // Clean up camera resources when the app is closed
    $(window).on("unload", function() {
      if (stream) {
        stream.getTracks().forEach(track => track.stop());
      }
    });